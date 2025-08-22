package com.company.minio2.service.minio.impl;

import com.company.minio2.dto.FileDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.exception.MinioException;
import com.company.minio2.service.minio.File2Service;
import io.minio.ListObjectsArgs;
import io.minio.MinioClient;

import io.minio.Result;
import io.minio.messages.Item;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;


@Service
public class File2ServiceImpl implements File2Service {


    private final MinioClient mc;

    public File2ServiceImpl(MinioClient mc) {
        this.mc = mc;
    }

    @Override
    public List<FileDto> openFolder(String bucket , String prefix) {
        return listLevel(bucket, prefix);
    }
    @Override
    public List<FileDto> listLevel(String bucket, String prefix) {
        final String p = normalizePrefix(prefix); // "" hoặc "Hihi/" hoặc "abc/"
        try {
            Iterable<Result<Item>> items = mc.listObjects(
                    ListObjectsArgs.builder()
                            .bucket(bucket)
                            .prefix(p)
                            .delimiter("/")
                            .recursive(false)
                            .build()
            );

            List<FileDto> out = new ArrayList<>();
            for (Result<Item> r : items) {
                Item it = r.get();
                String key = it.objectName();
                if (key == null || key.isBlank()) continue;

                FileDto dto = new FileDto();
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
