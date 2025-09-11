package com.company.minio2.service.minio;

import com.company.minio2.entity.Permission;
import com.company.minio2.entity.PermissionType;
import com.company.minio2.entity.User;
import io.jmix.core.DataManager;
import io.jmix.securitydata.entity.ResourceRoleEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collection;

@Service
public class SecurityService {

    @Autowired
    private DataManager dataManager;

    public void savePermission(Collection<Permission> permissions, User user, String filePath) {
        int mask = 0;
        for (Permission p : permissions) {
            if (Boolean.TRUE.equals(p.getAllow())) {
                if (p.getPermissionType() == PermissionType.FULL) {
                    mask = PermissionType.FULL.getValue(); // chỉ FULL thôi
                    break;
                }
                mask |= p.getPermissionType().getValue();
            }
        }
        Permission permission = loadPermission(user, filePath);
        if (permission == null) {
            permission = dataManager.create(Permission.class);
            permission.setUser(user);
            permission.setFilePath(filePath);
        }
        permission.setPermissionMask(mask);
        dataManager.save(permission);
    }

    public Permission loadPermission(User user, String filePath) {
        return dataManager.load(Permission.class)
                .query("select p from Permission p where p.user = :user and p.filePath = :filePath")
                .parameter("user", user)
                .parameter("filePath", filePath)
                .optional()
                .orElse(null);
    }


    public void savePermission(Collection<Permission> permissions, ResourceRoleEntity role, String filePath) {
        int mask = 0;
        for (Permission p : permissions) {
            if (Boolean.TRUE.equals(p.getAllow())) {
                if (p.getPermissionType() == PermissionType.FULL) {
                    mask = PermissionType.FULL.getValue(); // chỉ FULL thôi
                    break;
                }
                mask |= p.getPermissionType().getValue();
            }
        }
        Permission permission = loadPermission(role, filePath);
        if (permission == null) {
            permission = dataManager.create(Permission.class);
            permission.setRoleCode(role.getCode());
            permission.setFilePath(filePath);
        }
        permission.setPermissionMask(mask);
        dataManager.save(permission);
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

        int mask = perm.getPermissionMask();

        // Nếu có FULL (8) thì coi như có tất cả quyền
        if ((mask & 8) == 8) {
            return true;
        }

        switch (type) {
            case READ:
                return (mask & 1) == 1;
            case CREATE:
                return (mask & 2) == 2;
            case MODIFY:
                return (mask & 4) == 4;
            default:
                return false;
        }
    }
}

