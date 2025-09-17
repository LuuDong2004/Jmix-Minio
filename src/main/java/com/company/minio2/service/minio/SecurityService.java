package com.company.minio2.service.minio;

import com.company.minio2.dto.ObjectDto;
import com.company.minio2.dto.TreeNode;
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
        permission.setPermissionMask(mask);
        dataManager.save(permission);

        if (filePath != null && filePath.endsWith("/")) {
            String[] bp = splitBucketAndPrefix(filePath);
            String bucketName = bp[0];
            String prefix = bp[1]; // phần sau bucket
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
                .query("select p from Permission p where p.user = :user and p.filePath = :filePath")
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
            dataManager.save(perm);
            // nếu là folder thì đệ quy
            if (child.getType() == TreeNode.FOLDER) {
                String childPrefix = normalizePrefix(childKey);
                propagateToChildren(user, role, bucketName, childPrefix, parentMask);
            }
        }
    }

}
