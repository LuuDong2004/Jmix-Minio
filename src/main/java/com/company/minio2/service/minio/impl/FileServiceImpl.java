package com.company.minio2.service.minio.impl;

import com.company.minio2.config.MinioStorageProperties;
import com.company.minio2.dto.FileDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.exception.MinioException;
import com.company.minio2.service.minio.FileService;
import io.minio.ListObjectsArgs;
import io.minio.MinioClient;
import io.minio.Result;
import io.minio.messages.Item;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class FileServiceImpl implements FileService {

    private final MinioClient mc;
    private final MinioStorageProperties properties;


    public FileServiceImpl(MinioClient mc, MinioStorageProperties properties) {
        this.mc = mc;
        this.properties = properties;
    }

    @Override
    public List<FileDto> getAllFromBucket(String bucket,String prefix){

        try{
            String p = normalizePrefix(prefix);
            Iterable<Result<Item>> item = mc.listObjects(
                    ListObjectsArgs.builder()
                            .bucket(bucket)
                            .prefix(p)
                            .delimiter("/")   // ✅ thêm dòng này
                            .recursive(false)
                            .build()
            );
            List<FileDto> listFile = new ArrayList<>();
            for (Result<Item> r : item) {
                Item it = r.get();
                String key = it.objectName();
                if (key == null || key.isBlank()) continue;
                FileDto file = new FileDto();
                file.setName(extractDisplayName(p, key));
                file.setType(it.isDir() ? TreeNode.FOLDER : TreeNode.FILE);
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
