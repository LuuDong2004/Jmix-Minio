package com.company.minio2.service.minio;

import com.company.minio2.entity.Permission;
import com.company.minio2.entity.User;
import io.jmix.core.DataManager;
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



}
