package com.company.minio2.entity;

import io.jmix.core.metamodel.datatype.EnumClass;

public enum AppliesTo implements EnumClass<String> {

    THIS_FOLDER_ONLY("THIS FOLDER ONLY"),
    THIS_FOLDER_SUBFOLDERS_FILES("THIS FOLDER, SUBFOLDERS, FILES"),
    THIS_FOLDER_SUBFOLDERS("THIS FOLDER, SUBFOLDERS"),
    THIS_FOLDER_FILES("THIS FOLDER, FILES"),
    SUBFOLDERS_FILES_ONLY("SUBFOLDERS, FILES_ONLY");

    private final String id;

    AppliesTo(String id) {
        this.id = id;
    }

    @Override
    public String getId() {
        return "";
    }

    public static AppliesTo fromId(String id) {
        for (AppliesTo a : AppliesTo.values()) {
            if (a.getId().equals(id)) {
                return a;
            }
        }
        return null;
    }
}
