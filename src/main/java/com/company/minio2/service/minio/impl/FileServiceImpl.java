package com.company.minio2.service.minio.impl;

import com.company.minio2.config.MinioStorageProperties;
import com.company.minio2.dto.ObjectDto;
import com.company.minio2.exception.MinioException;
import com.company.minio2.service.minio.IFileService;
import io.minio.ListObjectsArgs;
import io.minio.MinioClient;
import io.minio.Result;
import io.minio.messages.Item;
import org.springframework.stereotype.Service;


import java.util.ArrayList;
import java.util.List;

@Service
public class FileServiceImpl implements IFileService {
    private final MinioClient minioClient;
    private final MinioStorageProperties properties;


    public FileServiceImpl(MinioClient minioClient, MinioStorageProperties properties) {
        this.minioClient = minioClient;
        this.properties = properties;
    }

    @Override
    public List<ObjectDto> getAllFromBucket(String bucket, String prefix){
            String p = normalizePrefix(prefix);
        try{
            Iterable<Result<Item>> item = minioClient.listObjects(
                    ListObjectsArgs.builder()
                            .bucket(bucket)
                            .prefix(p)
                            .delimiter("/")
                            .recursive(false)
                            .build()
            );
            List<ObjectDto> listFile = new ArrayList<>();
            for (Result<Item> r : item) {
                Item it = r.get();
                String key = it.objectName();
                if (key == null || key.isBlank()) continue;
                ObjectDto file = new ObjectDto();
                //String p = normalizePrefix(prefix);
                file.setName(extractDisplayName(p, key));
                file.setType(it.isDir() ? "Folder" : "File");
                file.setSize(it.isDir() ? null : it.size());
                file.setLastModified(
                        (it.isDir() || it.lastModified() == null) ? null : it.lastModified().toLocalDateTime()
                );
                listFile.add(file);
            }
            return listFile;
        }catch (Exception e){
            throw new MinioException("Danh sách file của bucket " + bucket + " hiển thị lỗi!", e);
        }
    }
    @Override
    public List<ObjectDto> listChildren(String bucket, String prefix) {
        String p = normalizePrefix(prefix);
        try {
            Iterable<Result<Item>> rs = minioClient.listObjects(
                    ListObjectsArgs.builder()
                            .bucket(bucket)
                            .prefix(p)
                            .recursive(false) // chỉ cấp ngay dưới
                            .build()
            );
            List<ObjectDto> out = new ArrayList<>();
            for (Result<Item> r : rs) {
                Item it = r.get();
                String key = it.objectName();
                if (key == null || key.isBlank()) continue;

                ObjectDto file = new ObjectDto();
                file.setKey(key);                               // full key để điều hướng / tải về
                file.setName(extractDisplayName(p, key));       // nhãn hiển thị
                file.setType(it.isDir() ? "Folder" : "File");
                file.setSize(it.isDir() ? null : it.size());
                file.setLastModified(it.isDir() || it.lastModified()==null
                        ? null : it.lastModified().toLocalDateTime());
                out.add(file);
            }
            return out;
        } catch (Exception e) {
            throw new RuntimeException("List children lỗi: " + p, e);
        }
    }


    private static String normalizePrefix(String prefix) {
        if (prefix == null || prefix.isBlank()) return "";
        return prefix.endsWith("/") ? prefix : (prefix + "/");
    }


    private static String extractDisplayName(String prefix, String key) {

        String rest = key.substring(prefix == null ? 0 : prefix.length());
        if (rest.endsWith("/")) rest = rest.substring(0, rest.length() - 1); // bỏ "/" nếu là folder
        int idx = rest.lastIndexOf('/');
        String name = (idx >= 0) ? rest.substring(idx + 1) : rest;
        return name.trim(); // phòng tên bị lẫn khoảng trắng
    }







}
