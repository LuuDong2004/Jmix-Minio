package com.company.minio2.service.minio;

import com.company.minio2.dto.ObjectDto;

import java.util.List;

public interface IFileService {
    List<ObjectDto> getAllFromBucket(String bucket, String prefix);
    List<ObjectDto> openFolder(String bucket, String prefix);
    List<ObjectDto> listLevel(String bucket, String prefix);
    List<ObjectDto> back(String bucket, String currentPrefix);
    String parentPrefix(String prefix);
}
