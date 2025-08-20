package com.company.minio2.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "minio")
public record MinioStorageProperties(
        String url,
        String accessKey,
        String secretKey,
        String bucket,
        int presignExpirySeconds // TTL mặc định cho URL ký sẵn (giây)
) {

}