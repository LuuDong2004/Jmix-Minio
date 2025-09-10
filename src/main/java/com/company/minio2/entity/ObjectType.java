package com.company.minio2.entity;

import io.jmix.core.metamodel.datatype.EnumClass;

import org.springframework.lang.Nullable;


public enum ObjectType implements EnumClass<String> {

    USER("A"),
    ROLE("B");

    private final String id;

    ObjectType(String id) {
        this.id = id;
    }

    public String getId() {
        return id;
    }

    @Nullable
    public static ObjectType fromId(String id) {
        for (ObjectType at : ObjectType.values()) {
            if (at.getId().equals(id)) {
                return at;
            }
        }
        return null;
    }
}