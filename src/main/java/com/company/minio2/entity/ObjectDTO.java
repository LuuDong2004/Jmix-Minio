package com.company.minio2.entity;

import io.jmix.core.entity.annotation.JmixId;
import io.jmix.core.metamodel.annotation.JmixEntity;

@JmixEntity
public class ObjectDTO {

    @JmixId
    private String id;
    private ObjectType type;
    private String name;

    private Boolean selected;

    public ObjectDTO() {
    }

    public ObjectDTO(String id, ObjectType type, String name) {
        this.id = id;
        this.type = type;
        this.name = name;
    }

    public Boolean getSelected() {
        return selected;
    }

    public void setSelected(Boolean selected) {
        this.selected = selected;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public ObjectType getType() {
        return type;
    }

    public void setType(ObjectType type) {
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
