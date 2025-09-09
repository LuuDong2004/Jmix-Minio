package com.company.minio2.dto;

import io.jmix.core.metamodel.annotation.JmixEntity;

import java.io.InputStream;
import java.util.function.Supplier;

@JmixEntity(name = "minio_download")
public class DownloadDTO {
    private String fileName;
    private String contentType;
    private Long size;
    private Supplier<InputStream> streamSupplier;

    public DownloadDTO() {
    }

    public DownloadDTO(String fileName, String contentType, Long size, Supplier<InputStream> streamSupplier) {
        this.fileName = fileName;
        this.contentType = contentType;
        this.size = size;
        this.streamSupplier = streamSupplier;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public String getContentType() {
        return contentType;
    }

    public void setContentType(String contentType) {
        this.contentType = contentType;
    }

    public long getSize() {
        return size;
    }

    public void setSize(Long size) {
        this.size = size;
    }

    public Supplier<InputStream> getStreamSupplier() {
        return streamSupplier;
    }

    public void setStreamSupplier(Supplier<InputStream> streamSupplier) {
        this.streamSupplier = streamSupplier;
    }
}
