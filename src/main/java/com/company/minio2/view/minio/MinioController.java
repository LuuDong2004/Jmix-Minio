package com.company.minio2.view.minio;

import com.company.minio2.dto.ObjectDto;
import com.company.minio2.service.minio.IFileService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

@RestController
@RequestMapping("/api/minio")
public class MinioController {

    private final IFileService fileService;

    public MinioController(IFileService fileService) {
        this.fileService = fileService;
    }

    @PostMapping(value = "/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ObjectDto> upload(@RequestParam String bucket,
                                            @RequestParam(required = false) String prefix,
                                            @RequestPart("file") MultipartFile file) {
        try (InputStream in = file.getInputStream()) {
            String filename = (file.getOriginalFilename() != null && !file.getOriginalFilename().isBlank())
                    ? file.getOriginalFilename() : "unnamed";

            String normalizedPrefix = (prefix == null || prefix.isBlank())
                    ? "" : (prefix.endsWith("/") ? prefix : prefix + "/");

            String objectKey = normalizedPrefix + filename;

            String contentType = (file.getContentType() != null && !file.getContentType().isBlank())
                    ? file.getContentType() : MediaType.APPLICATION_OCTET_STREAM_VALUE;

            ObjectDto dto = fileService.uploadFile(
                    bucket,
                    objectKey,
                    in,
                    file.getSize(),
                    contentType
            );
            return ResponseEntity.ok(dto);
        } catch (Exception e) {
            return ResponseEntity.badRequest().build();
        }
    }
    @DeleteMapping(value = "/delete", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Map<String, Object>> delete(
            @RequestParam String bucket,
            @RequestParam String objectKey) {
        try {
            fileService.delete(bucket, objectKey);
            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("bucket", bucket);
            response.put("objectKey", objectKey);
            response.put("message", objectKey.endsWith("/") ? "Đã xóa folder" : "Đã xóa file");
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            Map<String, Object> err = new HashMap<>();
            err.put("success", false);
            err.put("bucket", bucket);
            err.put("objectKey", objectKey);
            err.put("error", e.getMessage());
            return ResponseEntity.badRequest().body(err);
        }
    }

}