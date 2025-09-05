package com.company.minio2.entity;

import io.jmix.core.entity.annotation.JmixGeneratedValue;
import io.jmix.core.metamodel.annotation.JmixEntity;
import jakarta.persistence.*;

import java.util.UUID;

@JmixEntity
@Table(name = "PERMISSION", indexes = {
        @Index(name = "IDX_PERMISSION_USER", columnList = "USER_ID")
})
@Entity
public class Permission {
    @JmixGeneratedValue
    @Column(name = "ID", nullable = false)
    @Id
    private UUID id;

    @Column(name = "PERMISSION_TYPE")
    private Integer permissionType;

    @JoinColumn(name = "USER_ID")
    @ManyToOne(fetch = FetchType.LAZY)
    private User user;

    @Column(name = "FILE_PATH")
    private String filePath;

    @Column(name = "PERMISSION_MASK")
    private Integer permissionMask;

    @Column(name = "ROLE_CODE")
    private String roleCode;

    @Transient
    private Boolean allow;

    public String getRoleCode() {
        return roleCode;
    }

    public void setRoleCode(String roleCode) {
        this.roleCode = roleCode;
    }

    public void setPermissionType(PermissionType permissionType) {
        this.permissionType = permissionType == null ? null : permissionType.getId();
    }

    public PermissionType getPermissionType() {
        return permissionType == null ? null : PermissionType.fromId(permissionType);
    }

    public Boolean getAllow() {
        return allow;
    }

    public void setAllow(Boolean allow) {
        this.allow = allow;
    }

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public Integer getPermissionMask() {
        return permissionMask;
    }

    public void setPermissionMask(Integer permission) {
        this.permissionMask = permission;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }


}