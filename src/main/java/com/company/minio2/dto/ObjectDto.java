package com.company.minio2.dto;

import io.jmix.core.entity.annotation.JmixId;
import io.jmix.core.metamodel.annotation.JmixEntity;
import io.jmix.core.metamodel.annotation.JmixProperty;

import java.time.LocalDateTime;
import java.util.List;

@JmixEntity(name = "minio_file")
public class ObjectDto {
    @JmixId
    private String key;

    private List<ObjectDto> children;

    @JmixProperty(mandatory = true)
    private Long size;
    @JmixProperty(mandatory = true)
    private String type;
    @JmixProperty(mandatory = true)
    private LocalDateTime lastModified;
    @JmixProperty(mandatory = true)
    private String name;

    private ObjectDto parent;
    @JmixProperty(mandatory = true)
    private String path;

    public List<ObjectDto> getChildren() {
        return children;
    }

    public void setChildren(List<ObjectDto> children) {
        this.children = children;
    }

    public ObjectDto(String key, List<ObjectDto> children, Long size, String type, LocalDateTime lastModified, String name, ObjectDto parent, String path) {
        this.key = key;
        this.children = children;
        this.size = size;
        this.type = type;
        this.lastModified = lastModified;
        this.name = name;
        this.parent = parent;
        this.path = path;
    }

    public ObjectDto() {
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public Long getSize() {
        return size;
    }

    public void setSize(Long size) {
        this.size = size;
    }

    public void setType(TreeNode type) {
        this.type = type == null ? null : type.getId();
    }

    public TreeNode getType() {
        return type == null ? null : TreeNode.fromId(type);
    }

    public LocalDateTime getLastModified() {
        return lastModified;
    }

    public void setLastModified(LocalDateTime lastModified) {
        this.lastModified = lastModified;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public ObjectDto getParent() {
        return parent;
    }
    public void setParent(ObjectDto parent) {
        this.parent = parent;
    }
    public String getPath() {
        return path;
    }
    public void setPath(String path) {
        this.path = path;
    }

    public void setType(String type) {
        this.type = type;
    }
}
