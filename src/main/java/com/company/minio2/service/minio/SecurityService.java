package com.company.minio2.service.minio;

import com.company.minio2.dto.ObjectDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.entity.AppliesTo;
import com.company.minio2.entity.Permission;
import com.company.minio2.entity.PermissionType;
import com.company.minio2.entity.User;
import io.jmix.core.DataManager;
import io.jmix.securitydata.entity.ResourceRoleEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.List;

@Service
public class SecurityService {

    @Autowired
    private DataManager dataManager;

    @Autowired
    private IFileService fileService;

    public void savePermission(Collection<Permission> permissions, User user, String filePath) {
        normalizePermissions(permissions);
        int mask = buildMask(permissions);

        Permission permission = loadPermission(user, filePath);
        if (permission == null) {
            permission = dataManager.create(Permission.class);
            permission.setUser(user);
            permission.setFilePath(filePath);
        }

        // Xác định AppliesTo mặc định
        AppliesTo appliesTo;
        if (filePath != null && filePath.endsWith("/")) {
            appliesTo = AppliesTo.THIS_FOLDER_SUBFOLDERS_FILES;
        } else {
            appliesTo = AppliesTo.THIS_FOLDER_ONLY;
        }
        permission.setAppliesTo(appliesTo);

        permission.setPermissionMask(mask);
        permission.setInherited(false);
        dataManager.save(permission);

        // propagate nếu là folder
        if (filePath != null && filePath.endsWith("/")) {
            String[] bp = splitBucketAndPrefix(filePath);
            String bucketName = bp[0];
            String prefix = bp[1];
            String normalizedPrefix = normalizePrefix(prefix);
            propagateToChildren(user, null, bucketName, normalizedPrefix, mask);
        }
    }

    public void savePermission(Collection<Permission> permissions, ResourceRoleEntity role, String filePath) {
        normalizePermissions(permissions);
        int mask = buildMask(permissions);

        Permission permission = loadPermission(role, filePath);
        if (permission == null) {
            permission = dataManager.create(Permission.class);
            permission.setRoleCode(role.getCode());
            permission.setFilePath(filePath);
        }

        if (filePath != null && filePath.endsWith("/")) {
            // folder
            permission.setAppliesTo(AppliesTo.THIS_FOLDER_SUBFOLDERS_FILES);
        } else {
            // file
            permission.setAppliesTo(AppliesTo.THIS_FOLDER_ONLY);
        }

        permission.setPermissionMask(mask);
        dataManager.save(permission);

        if (filePath != null && filePath.endsWith("/")) {
            String[] bp = splitBucketAndPrefix(filePath);
            String bucketName = bp[0];
            String prefix = bp[1];
            String normalizedPrefix = normalizePrefix(prefix);
            propagateToChildren(null, role, bucketName, normalizedPrefix, mask);
        }
    }

    public Permission loadPermission(User user, String filePath) {
        return dataManager.load(Permission.class)
                .query("select p from Permission p join fetch p.user where p.user = :user and p.filePath = :filePath")
                .parameter("user", user)
                .parameter("filePath", filePath)
                .optional()
                .orElse(null);
    }


    public Permission loadPermission(ResourceRoleEntity role, String filePath) {
        return dataManager.load(Permission.class)
                .query("select p from Permission p where p.roleCode = :roleCode and p.filePath = :filePath")
                .parameter("roleCode", role.getCode())
                .parameter("filePath", filePath)
                .optional()
                .orElse(null);
    }

    public boolean hasPermission(User user, PermissionType type, String filePath) {
        Permission perm = dataManager.load(Permission.class)
                .query("select p from Permission p where p.user = :user and :filePath like concat(p.filePath, '%')")
                .parameter("user", user)
                .parameter("filePath", filePath)
                .optional()
                .orElse(null);

        if (perm == null) {
            return false;
        }
        int mask = perm.getPermissionMask() == null ? 0 : perm.getPermissionMask();
        if ((mask & PermissionType.FULL.getValue()) == PermissionType.FULL.getValue()) {
            return true;
        }
        switch (type) {
            case READ:
                return (mask & PermissionType.READ.getValue()) == PermissionType.READ.getValue();
            case CREATE:
                return (mask & PermissionType.CREATE.getValue()) == PermissionType.CREATE.getValue();
            case MODIFY:
                return (mask & PermissionType.MODIFY.getValue()) == PermissionType.MODIFY.getValue();
            default:
                return false;
        }
    }

    public boolean hasPermission(String username, PermissionType type, String bucket, String prefix) {
        if (username == null || bucket == null) return false;

        User user = dataManager.load(User.class)
                .query("select u from User u where u.username = :username")
                .parameter("username", username)
                .optional()
                .orElse(null);

        if (user == null) return false;

        String filePath = bucket + (prefix == null || prefix.isBlank() ? "" : "/" + prefix);
        return hasPermission(user, type, filePath);
    }


    private void normalizePermissions(Collection<Permission> permissions) {
        boolean readDenied = permissions.stream()
                .anyMatch(p -> p.getPermissionType() == PermissionType.READ && Boolean.FALSE.equals(p.getAllow()));

        if (readDenied) {
            for (Permission p : permissions) {
                if (p.getPermissionType() == PermissionType.CREATE
                        || p.getPermissionType() == PermissionType.MODIFY) {
                    p.setAllow(false);
                }
            }
        }

        boolean hasFull = permissions.stream()
                .anyMatch(p -> p.getPermissionType() == PermissionType.FULL && Boolean.TRUE.equals(p.getAllow()));

        if (hasFull) {
            for (Permission p : permissions) {
                if (p.getPermissionType() != PermissionType.FULL) {
                    p.setAllow(false);
                }
            }
        }
    }

    private int buildMask(Collection<Permission> permissions) {
        int mask = 0;
        for (Permission p : permissions) {
            if (Boolean.TRUE.equals(p.getAllow())) {
                if (p.getPermissionType() == PermissionType.FULL) {
                    return PermissionType.FULL.getValue();
                }
                mask |= p.getPermissionType().getValue();
            }
        }
        return mask;
    }

    private String[] splitBucketAndPrefix(String filePath) {
        if (filePath == null || filePath.isBlank()) {
            return new String[]{"", ""};
        }
        int idx = filePath.indexOf('/');
        if (idx < 0) {
            // chỉ có bucket, không có prefix
            return new String[]{filePath, ""};
        }
        String bucket = filePath.substring(0, idx);
        String prefix = filePath.substring(idx + 1); // phần sau bucket/
        return new String[]{bucket, prefix};
    }

    private static String normalizePrefix(String prefix) {
        if (prefix == null || prefix.isBlank()) return "";
        return prefix.endsWith("/") ? prefix : prefix + "/";
    }

    // kế thừa từ cha (có xử lý Deny override Allow)
    private void propagateToChildren(User user, ResourceRoleEntity role, String bucketName, String parentPath, int parentMask) {
        String normalizedParent = normalizePrefix(parentPath);
        List<ObjectDto> children = fileService.listLevel(bucketName, normalizedParent);
        for (ObjectDto child : children) {
            String childKey = child.getKey();
            String childFullPath = bucketName + "/" + childKey;
            Permission perm;
            if (user != null) {
                perm = loadPermission(user, childFullPath);
            } else {
                perm = loadPermission(role, childFullPath);
            }

            if (perm != null && Boolean.FALSE.equals(perm.getInheritEnabled())) {
                continue;
            }

            int currentMask = (perm == null || perm.getPermissionMask() == null) ? 0 : perm.getPermissionMask();
            int newMask = currentMask;
            // Nếu cha FULL → override hoàn toàn
            if ((parentMask & PermissionType.FULL.getValue()) == PermissionType.FULL.getValue()) {
                newMask = PermissionType.FULL.getValue();
            } else {
                // Với từng quyền READ / CREATE / MODIFY
                for (PermissionType pt : PermissionType.values()) {
                    if (pt == PermissionType.FULL) continue; // FULL xử lý riêng ở trên
                    int bit = pt.getValue();
                    if ((parentMask & bit) == bit) {
                        // Cha Allow → thêm quyền vào con
                        newMask |= bit;
                    } else {
                        // Cha Deny → xóa quyền ở con
                        newMask &= ~bit;
                    }
                }
            }
            if (perm == null) {
                perm = dataManager.create(Permission.class);
                if (user != null) {
                    perm.setUser(user);
                } else {
                    perm.setRoleCode(role.getCode());
                }
                perm.setFilePath(childFullPath);
            }
            perm.setPermissionMask(newMask);
            perm.setInherited(true);      // đánh dấu là kế thừa
            perm.setInheritEnabled(true); // vẫn cho phép kế thừa tiếp
            perm.setInheritedFrom(bucketName + "/" + normalizedParent);

            if (child.getType() == TreeNode.FOLDER) {
                perm.setAppliesTo(AppliesTo.THIS_FOLDER_SUBFOLDERS_FILES);
            } else {
                perm.setAppliesTo(AppliesTo.THIS_FOLDER_ONLY);
            }

            dataManager.save(perm);
            // nếu là folder thì đệ quy
            if (child.getType() == TreeNode.FOLDER) {
                String childPrefix = normalizePrefix(childKey);
                propagateToChildren(user, role, bucketName, childPrefix, parentMask);
            }
        }
    }

    public void disableInheritance(User user, String filePath, boolean convertToExplicit) {
        // Lấy tất cả permission trên filePath (cả explicit và inherited)
        List<Permission> permissions = dataManager.load(Permission.class)
                .query("select p from Permission p where p.user = :user and p.filePath = :filePath")
                .parameter("user", user)
                .parameter("filePath", filePath)
                .list();

        for (Permission perm : permissions) {
            // Tắt inheritance cho object
            perm.setInheritEnabled(false);

            if (perm.getInherited()) {
                if (convertToExplicit) {
                    // Convert: biến thành explicit
                    perm.setInherited(false);
                    dataManager.save(perm);
                } else {
                    // Remove: xóa permission được kế thừa
                    dataManager.remove(perm);
                }
            } else {
                // Explicit thì giữ nguyên
                dataManager.save(perm);
            }
        }
    }

    public void enableInheritance(User user, String filePath) {
        // Lấy tất cả permission (explicit hoặc inherited nếu còn) cho user tại node này
        List<Permission> perms = dataManager.load(Permission.class)
                .query("select p from Permission p where p.user = :user and p.filePath = :path")
                .parameter("user", user)
                .parameter("path", filePath)
                .list();

        // bật inheritEnabled cho các bản đã tồn tại
        for (Permission p : perms) {
            p.setInheritEnabled(true);
            dataManager.save(p);
        }

        // tìm ancestor gần nhất có permission cho user này
        String parentPath = findParentPath(filePath);
        Permission ancestor = findNearestAncestorPermission(user, parentPath);

        if (ancestor != null) {
            // Nếu node hiện tại chưa có permission cho user này thì tạo bản inherited mới
            Permission nodePerm = dataManager.load(Permission.class)
                    .query("select p from Permission p where p.user = :user and p.filePath = :path")
                    .parameter("user", user)
                    .parameter("path", filePath)
                    .optional()
                    .orElse(null);

            if (nodePerm == null) {
                nodePerm = dataManager.create(Permission.class);
                nodePerm.setUser(user);
                nodePerm.setFilePath(filePath);
                nodePerm.setPermissionMask(ancestor.getPermissionMask());
                nodePerm.setInherited(true);
                nodePerm.setInheritEnabled(true);
                nodePerm.setInheritedFrom(ancestor.getFilePath());
                nodePerm.setAppliesTo(filePath != null && filePath.endsWith("/")
                        ? AppliesTo.THIS_FOLDER_SUBFOLDERS_FILES
                        : AppliesTo.THIS_FOLDER_ONLY);
                dataManager.save(nodePerm);
            }

            // propagate xuống children theo mask của ancestor (đảm bảo dùng bucket/prefix đúng)
            String[] bp = splitBucketAndPrefix(filePath);
            String bucket = bp[0];
            String prefix = bp[1];
            propagateToChildren(user, null, bucket, prefix, ancestor.getPermissionMask());
        }
    }

    /**
     * Enable inheritance for a node when there is NO permission record on the node (Remove case).
     * It finds the nearest ancestor that has any permissions and clones all those permissions
     * as inherited permissions on this node (for all users and roles), then propagates down.
     */
    public void enableRemoveInheritance(String filePath) {
        if (filePath == null) return;

        String parentPath = findParentPath(filePath);
        String[] bp = splitBucketAndPrefix(filePath);
        String bucket = bp[0];
        String prefix = bp[1];
        String normalizedPrefix = normalizePrefix(prefix);

        String current = parentPath;
        while (current != null && !current.isEmpty()) {
            List<Permission> ancestorPerms = dataManager.load(Permission.class)
                    .query("select p from Permission p where p.filePath = :path")
                    .parameter("path", current)
                    .list();

            if (!ancestorPerms.isEmpty()) {
                // clone mỗi permission vào node hiện tại (nếu đã có permission cùng principal thì chỉ bật inheritEnabled)
                for (Permission anc : ancestorPerms) {
                    Permission existing;
                    if (anc.getUser() != null) {
                        existing = dataManager.load(Permission.class)
                                .query("select p from Permission p where p.user = :user and p.filePath = :path")
                                .parameter("user", anc.getUser())
                                .parameter("path", filePath)
                                .optional().orElse(null);
                    } else {
                        existing = dataManager.load(Permission.class)
                                .query("select p from Permission p where p.roleCode = :roleCode and p.filePath = :path")
                                .parameter("roleCode", anc.getRoleCode())
                                .parameter("path", filePath)
                                .optional().orElse(null);
                    }

                    if (existing != null) {
                        existing.setInheritEnabled(true);
                        dataManager.save(existing);
                    } else {
                        Permission inher = dataManager.create(Permission.class);
                        if (anc.getUser() != null) {
                            inher.setUser(anc.getUser());
                        } else {
                            inher.setRoleCode(anc.getRoleCode());
                        }
                        inher.setFilePath(filePath);
                        inher.setPermissionMask(anc.getPermissionMask());
                        inher.setInherited(true);
                        inher.setInheritEnabled(true);
                        inher.setInheritedFrom(anc.getFilePath());
                        inher.setAppliesTo(filePath.endsWith("/") ? AppliesTo.THIS_FOLDER_SUBFOLDERS_FILES : AppliesTo.THIS_FOLDER_ONLY);
                        dataManager.save(inher);
                    }

                    // propagate tương ứng cho mỗi principal
                    if (anc.getUser() != null) {
                        propagateToChildren(anc.getUser(), null, bucket, normalizedPrefix, anc.getPermissionMask());
                    } else {
                        ResourceRoleEntity roleEntity = loadRoleByCode(anc.getRoleCode());
                        if (roleEntity != null) {
                            propagateToChildren(null, roleEntity, bucket, normalizedPrefix, anc.getPermissionMask());
                        }
                    }
                }
                // đã clone từ ancestor gần nhất -> dừng
                return;
            }

            current = findParentPath(current);
        }
        // nếu không tìm được ancestor có permission thì không làm gì
    }

    private ResourceRoleEntity loadRoleByCode(String roleCode) {
        if (roleCode == null) return null;
        return dataManager.load(ResourceRoleEntity.class)
                .query("select r from ResourceRoleEntity r where r.code = :code")
                .parameter("code", roleCode)
                .optional()
                .orElse(null);
    }


    private Permission findNearestAncestorPermission(User user, String startPath) {
        String current = startPath;
        while (current != null && !current.isEmpty()) {
            Permission p = dataManager.load(Permission.class)
                    .query("select p from Permission p where p.user = :user and p.filePath = :path")
                    .parameter("user", user)
                    .parameter("path", current)
                    .optional()
                    .orElse(null);
            if (p != null) {
                return p;
            }
            current = findParentPath(current);
        }
        return null;
    }

    private String findParentPath(String filePath) {
        if (filePath == null || !filePath.contains("/")) {
            return ""; // root thì không có cha
        }
        // Bỏ dấu '/' cuối (nếu có)
        String normalized = filePath.endsWith("/") ? filePath.substring(0, filePath.length() - 1) : filePath;
        int lastSlash = normalized.lastIndexOf('/');
        if (lastSlash < 0) {
            return ""; // không có cha
        }
        return normalized.substring(0, lastSlash + 1); // giữ lại '/' cuối
    }

    public void replaceChildPermissions(User user, String parentPath, int parentMask) {
        String[] bp = splitBucketAndPrefix(parentPath);
        String bucket = bp[0];
        String prefix = bp[1];
        String normalizedPrefix = normalizePrefix(prefix);

        List<ObjectDto> children = fileService.listLevel(bucket, normalizedPrefix);
        for (ObjectDto child : children) {
            String childFullPath = bucket + "/" + child.getKey();

            // Xóa tất cả permission cũ của con
            List<Permission> existingPerms = dataManager.load(Permission.class)
                    .query("select p from Permission p where p.user = :user and p.filePath = :path")
                    .parameter("user", user)
                    .parameter("path", childFullPath)
                    .list();
            for (Permission e : existingPerms) {
                dataManager.remove(e);
            }

            // Tạo permission mới kế thừa từ cha
            Permission childPerm = dataManager.create(Permission.class);
            childPerm.setUser(user);
            childPerm.setFilePath(childFullPath);
            childPerm.setPermissionMask(parentMask);
            childPerm.setInherited(true);
            childPerm.setInheritEnabled(true);
            childPerm.setInheritedFrom(parentPath);

            if (child.getType() == TreeNode.FOLDER) {
                childPerm.setAppliesTo(AppliesTo.THIS_FOLDER_SUBFOLDERS_FILES);
            } else {
                childPerm.setAppliesTo(AppliesTo.THIS_FOLDER_ONLY);
            }
            dataManager.save(childPerm);
            // Đệ quy xuống tiếp nếu là folder
            if (child.getType() == TreeNode.FOLDER) {
                replaceChildPermissions(user, childFullPath, parentMask);
            }
        }
    }

}
