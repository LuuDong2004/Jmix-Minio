package com.company.minio2.security;

import io.jmix.security.role.annotation.ResourceRole;

@ResourceRole(name = "UserManagerRole", code = UserManagerRole.CODE)
public interface UserManagerRole {
    String CODE = "user-manager-role";
}