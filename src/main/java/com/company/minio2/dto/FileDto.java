package com.company.minio2.dto;

import io.jmix.core.entity.annotation.JmixId;
import io.jmix.core.metamodel.annotation.JmixEntity;
import io.jmix.core.metamodel.annotation.JmixProperty;

import java.time.LocalDateTime;
import java.util.List;

@JmixEntity(name = "minio_file")
public class FileDto {
    @JmixId
    private String key;

    private List<FileDto> children;

    @JmixProperty(mandatory = true)
    private Long size;
    @JmixProperty(mandatory = true)
    private String type;
    @JmixProperty(mandatory = true)
    private LocalDateTime lastModified;
    @JmixProperty(mandatory = true)
    private String name;

    public List<FileDto> getChildren() {
        return children;
    }

    public void setChildren(List<FileDto> children) {
        this.children = children;
    }

    public FileDto() {
    }

    public FileDto(String key, Long size, String type, LocalDateTime lastModified, String name) {
        this.key = key;
        this.size = size;
        this.type = type;
        this.lastModified = lastModified;
        this.name = name;
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
}
