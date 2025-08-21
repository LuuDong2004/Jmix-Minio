package com.company.minio2.service.minio.impl;

import com.company.minio2.dto.BucketDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.exception.MinioException;
import com.company.minio2.service.minio.IBucketService;
import io.minio.*;
import io.minio.messages.Item;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class BucketServiceImpl implements IBucketService {

    private final MinioClient mc;

    public BucketServiceImpl(MinioClient mc) {
        this.mc = mc;
    }

    @Override
    public List<BucketDto> getAllBuckets() {
        try {
            var buckets = mc.listBuckets(ListBucketsArgs.builder().build());
            List<BucketDto> bucket = new ArrayList<>();
            for (var b : buckets) {
                BucketDto node = new BucketDto();
                node.setId(b.name());
                node.setBucketName(b.name());
                node.setCreatDate(b.creationDate() == null ? null : b.creationDate().toLocalDate().toString());
                node.setPath("");
                node.setType(TreeNode.BUCKET);
                node.setParent(null);
                bucket.add(node);
            }
            return bucket;
        } catch (Exception e) {
            throw new MinioException("Không thể load buckets!" , e);
        }
    }

    //tree service
    @Override
    public List<BucketDto> listBucketFolderTree() {
        try {
            List<BucketDto> nodes = new ArrayList<>();
            Map<String, BucketDto> index = new HashMap<>();

            // 1) Root = các bucket
            for (BucketDto b : getAllBuckets()) {
                nodes.add(b);
                index.put(b.getId(), b);

                // 2) Lấy toàn bộ object trong bucket => suy ra folder ảo
                Iterable<Result<Item>> it = mc.listObjects(
                        ListObjectsArgs.builder()
                                .bucket(b.getBucketName())   // chính là tên bucket
                                .recursive(true)
                                .build()
                );

                for (Result<Item> r : it) {
                    Item item = r.get();
                    String key = item.objectName();
                    if (key == null || key.isBlank()) continue;

                    String[] parts = key.split("/");
                    String path = "";
                    BucketDto parent = b;


                    for (int i = 0; i < parts.length - 1; i++) {
                        String folder = parts[i];
                        path += folder + "/";
                        String id = b.getBucketName() + ":" + path;

                        BucketDto folderNode = index.get(id);
                        if (folderNode == null) {
                            folderNode = new BucketDto();
                            folderNode.setId(id);
                            folderNode.setBucketName(folder);
                            folderNode.setPath(path);
                            folderNode.setType(TreeNode.FOLDER);
                            folderNode.setParent(parent);
                            nodes.add(folderNode);
                            index.put(id, folderNode);
                        }
                        parent = folderNode;
                    }
                }
            }
            return nodes;
        } catch (Exception e) {
            throw new RuntimeException("Build bucket-folder tree failed", e);
        }
    }

    @Override
    public void removeBucket(String bucketName){
        try{
            mc.removeBucket(RemoveBucketArgs.builder().bucket(bucketName).build());
        }catch(Exception e){
            throw new MinioException("Không thể xóa bucket này!" , e);
        }
    }

}
