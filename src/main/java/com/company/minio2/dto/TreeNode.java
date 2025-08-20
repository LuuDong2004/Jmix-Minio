package com.company.minio2.dto;

import io.jmix.core.metamodel.datatype.EnumClass;

import org.springframework.lang.Nullable;


public enum TreeNode implements EnumClass<String> {
    BUCKET("B"),
    FOLDER("F");
    private final String id;

    TreeNode(String id) {
        this.id = id;
    }

    public String getId() {
        return id;
    }

    @Nullable
    public static TreeNode fromId(String id) {
        for (TreeNode at : TreeNode.values()) {
            if (at.getId().equals(id)) {
                return at;
            }
        }
        return null;
    }
}