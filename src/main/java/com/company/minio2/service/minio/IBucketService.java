package com.company.minio2.service.minio;

import com.company.minio2.dto.BucketDto;
import com.company.minio2.dto.FileDto;

import java.util.List;

public interface IBucketService {
    List<BucketDto> getAllBuckets();

    List<BucketDto> listBucketFolderTree();
    //void createBucket(String bucketName);
    void removeBucket(String bucketName);


}
