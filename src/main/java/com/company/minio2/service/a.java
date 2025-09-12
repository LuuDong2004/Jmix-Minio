package com.company.minio2.service;

public class a {
//    @Override
//    public void delete(String bucket, String objectKey) {
//        if (bucket == null || bucket.isBlank())
//            System.out.println("Bucket không hợp lệ");
//        if (objectKey == null || objectKey.isBlank())
//            System.out.println("Object key rỗng");
//        try {
//            if (objectKey.endsWith("/")) {
//                // XÓA FOLDER: xóa toàn bộ object có prefix = key
//                Iterable<Result<Item>> it = minioClient.listObjects(
//                        ListObjectsArgs.builder()
//                                .bucket(bucket)
//                                .prefix(objectKey)
//                                .recursive(true)
//                                .build()
//                );
//
//                List<DeleteObject> batch = new ArrayList<>();
//                for (Result<Item> r : it) {
//                    Item item = r.get();
//                    if (item.objectName() != null && !item.objectName().isBlank()) {
//                        batch.add(new DeleteObject(item.objectName()));
//                    }
//                }
//                // Xóa luôn “placeholder” folder (vd: "foo/")
//                batch.add(new DeleteObject(objectKey));
//
//                if (!batch.isEmpty()) {
//                    Iterable<Result<DeleteError>> results = minioClient.removeObjects(
//                            RemoveObjectsArgs.builder()
//                                    .bucket(bucket)
//                                    .objects(batch)
//                                    .build()
//                    );
//                    // Nếu MinIO trả về lỗi xóa một vài object
//                    for (Result<DeleteError> err : results) {
//                        DeleteError de = err.get();
//                        // throw new MinioException("Không thể xóa: " + de.objectName() + " - " + de.message());
//                    }
//                }
//            } else {
//                // XÓA FILE
//                minioClient.removeObject(
//                        RemoveObjectArgs.builder()
//                                .bucket(bucket)
//                                .object(objectKey)
//                                .build()
//                );
//            }
//        } catch (Exception e) {
//            throw new MinioException("Service không thể xóa!", e);
//        }
//    }
}
