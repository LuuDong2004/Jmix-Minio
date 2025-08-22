package com.company.minio2.service.minio;

import com.company.minio2.dto.FileDto;

import java.util.List;

public interface File2Service {
    List<FileDto> openFolder(String bucket , String prefix);
    List<FileDto> listLevel(String bucket, String prefix);
}
