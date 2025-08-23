package com.company.minio2.service.minio.impl;

import com.company.minio2.config.MinioStorageProperties;
import com.company.minio2.dto.ObjectDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.exception.MinioException;
import com.company.minio2.service.minio.IFileService;
import io.minio.ListObjectsArgs;
import io.minio.MinioClient;
import io.minio.Result;
import io.minio.messages.Item;
import org.springframework.stereotype.Service;


import java.util.ArrayList;
import java.util.Comparator;
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
                file.setKey(key);
                file.setName(extractDisplayName(p, key));
                file.setType(it.isDir() ? TreeNode.FOLDER : TreeNode.FILE);
                file.setSize(it.isDir() ? null : it.size());
                file.setLastModified((it.isDir() || it.lastModified() == null) ? null : it.lastModified().toLocalDateTime());
                listFile.add(file);
            }
            return listFile;
        } catch (Exception e){
            throw new MinioException("Danh sách file của bucket " + bucket + " hiển thị lỗi!", e);
        }
    }


    @Override
    public List<ObjectDto> openFolder(String bucket , String prefix) {
        return listLevel(bucket, prefix);
    }
    @Override
    public List<ObjectDto> listLevel(String bucket, String prefix) {
        final String p = normalizePrefix(prefix);
        try {
            Iterable<Result<Item>> items = minioClient.listObjects(
                    ListObjectsArgs.builder()
                            .bucket(bucket)
                            .prefix(p)
                            .delimiter("/")
                            .recursive(false)
                            .build()
            );

            List<ObjectDto> out = new ArrayList<>();
            for (Result<Item> r : items) {
                Item it = r.get();
                String key = it.objectName();
                if (key == null || key.isBlank()) continue;

                ObjectDto dto = new ObjectDto();
                dto.setKey(key); // full path (vd: "Hihi/test2.txt" hoặc "Hihi/child/")
                dto.setName(extractDisplayName(p, key)); // chỉ lấy phần tên sau prefix
                dto.setType(it.isDir() ? TreeNode.FOLDER : TreeNode.FILE);
                dto.setSize(it.isDir() ? null : it.size());
                dto.setLastModified(
                        (it.isDir() || it.lastModified() == null)
                                ? null
                                : it.lastModified().toLocalDateTime()
                );
                out.add(dto);
            }
            return out;
        } catch (Exception e) {
            throw new MinioException("List level thất bại (bucket=" + bucket + ", prefix=" + p + ")", e);
        }
    }

    //quay lại object trước đó
    @Override
    public List<ObjectDto> back(String bucket, String currentPrefix) {
        String parent = parentPrefix(currentPrefix);
        return listLevel(bucket, parent);
    }
    @Override
    public String parentPrefix(String prefix) {
        if (prefix == null || prefix.isBlank()) return "";
        String p = prefix.endsWith("/") ? prefix.substring(0, prefix.length()-1) : prefix;
        int idx = p.lastIndexOf('/');
        return (idx >= 0) ? p.substring(0, idx + 1) : ""; // giữ "/" ở cuối nếu còn cha
    }

    //chuẩn hóa
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
