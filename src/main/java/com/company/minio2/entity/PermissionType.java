package com.company.minio2.entity;

import io.jmix.core.metamodel.datatype.EnumClass;

public enum PermissionType implements EnumClass<Integer> {
    READ(1),
    CREATE(2),
    MODIFY(4),
    FULL(8);

    private final int value;

    PermissionType(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public static boolean hasPermission(int mask, PermissionType type) {
        // Nếu đã có FULL thì pass hết
        if ((mask & FULL.value) == FULL.value) {
            return true;
        }
        // Kiểm tra bit tương ứng
        return (mask & type.value) == type.value;
    }

    @Override
    public Integer getId() {
        return value;
    }

    public static PermissionType fromId(Integer id) {
        if (id == null)
            return null;
        for (PermissionType type : PermissionType.values()) {
            if (type.getId().equals(id)) {
                return type;
            }
        }
        return null;
    }

    @Override
    public String toString() {
        switch (this) {
            case READ:
                return "READ";
            case CREATE:
                return "CREATE";
            case MODIFY:
                return "MODIFY";
            case FULL:
                return "FULL ACCESS";
            default:
                return super.toString();
        }
    }
}
