package com.company.minio2.entity;

public enum PermissionType {
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
}
