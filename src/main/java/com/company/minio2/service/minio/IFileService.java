package com.company.minio2.service.minio;

import com.company.minio2.dto.ObjectDto;

import java.util.List;

public interface IFileService {
    List<ObjectDto> getAllFromBucket(String bucket, String prefix);
    List<ObjectDto> listChildren(String bucket, String prefix);
}
