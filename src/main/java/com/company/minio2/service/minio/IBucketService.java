package com.company.minio2.service.minio;

import com.company.minio2.dto.BucketDto;

import java.util.List;

public interface IBucketService {
    List<BucketDto> getAllBuckets();
    List<BucketDto> listBucketFolderTree();
    void removeBucket(String bucketName);
    void createBucket(String bucketName);
}
