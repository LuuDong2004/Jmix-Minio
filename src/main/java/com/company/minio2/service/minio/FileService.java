package com.company.minio2.service.minio;

import com.company.minio2.dto.FileDto;

import java.util.List;

public interface FileService {
    List<FileDto> getAllFromBucket(String bucket, String prefix);
}
