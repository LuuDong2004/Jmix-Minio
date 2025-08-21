package com.company.minio2.dto;
import io.jmix.core.entity.annotation.JmixId;
import io.jmix.core.metamodel.annotation.JmixEntity;

import io.jmix.core.metamodel.annotation.JmixProperty;

import java.time.LocalDateTime;

@JmixEntity(name="minio_file")
public class ObjectDto {
    @JmixId
    private String key;
    @JmixProperty(mandatory = true)
    private Long size;
    @JmixProperty(mandatory = true)
    private String type;
    @JmixProperty(mandatory = true)
    private LocalDateTime lastModified;
    @JmixProperty(mandatory = true)
    private String name;

    public ObjectDto() {
    }

    public ObjectDto(String key, Long size, String type, LocalDateTime lastModified, String name) {
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

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
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
