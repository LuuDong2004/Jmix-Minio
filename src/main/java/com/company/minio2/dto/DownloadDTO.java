package com.company.minio2.dto;



import java.io.InputStream;
import java.util.function.Supplier;


public class DownloadDTO {
    private final String fileName;
    private final String contentType;
    private final Long size;
    private final Supplier<InputStream> streamSupplier;

    public DownloadDTO(String fileName, String contentType, Long size, Supplier<InputStream> streamSupplier) {
        this.fileName = fileName;
        this.contentType = contentType;
        this.size = size;
        this.streamSupplier = streamSupplier;
    }

    public String getFileName() { return fileName; }
    public String getContentType() { return contentType; }
    public Long getSize() { return size; }
    public Supplier<InputStream> getStreamSupplier() { return streamSupplier; }
}
