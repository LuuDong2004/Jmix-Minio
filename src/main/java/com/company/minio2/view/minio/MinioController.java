package com.company.minio2.view.minio;

import com.company.minio2.entity.PermissionType;
import com.company.minio2.entity.User;
import com.company.minio2.service.minio.SecurityService;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
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
    private final SecurityService securityService;

    public MinioController(IFileService fileService, SecurityService securityService) {
        this.fileService = fileService;
        this.securityService = securityService;
    }

    @PostMapping(value = "/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<?> upload(@AuthenticationPrincipal User user,
                                    @RequestParam String bucket,
                                    @RequestParam(required = false) String prefix,
                                    @RequestPart("file") MultipartFile file) {
        try (InputStream in = file.getInputStream()) {
            String filename = (file.getOriginalFilename() != null && !file.getOriginalFilename().isBlank())
                    ? file.getOriginalFilename() : "unnamed";

            String normalizedPrefix = (prefix == null || prefix.isBlank())
                    ? "" : (prefix.endsWith("/") ? prefix : prefix + "/");

            String objectKey = normalizedPrefix + filename;
            String filePath = bucket + "/" + objectKey;

            // 🚨 Check quyền
            if (!securityService.hasPermission(user, PermissionType.CREATE, filePath) &&
                    !securityService.hasPermission(user, PermissionType.FULL, filePath)) {
                return ResponseEntity.status(403).body("Bạn không có quyền upload: " + filePath);
            }

            ObjectDto dto = fileService.uploadFile(
                    bucket, objectKey, in, file.getSize(),
                    file.getContentType() != null ? file.getContentType() : MediaType.APPLICATION_OCTET_STREAM_VALUE
            );
            return ResponseEntity.ok(dto);
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }

    @DeleteMapping(value = "/delete", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<?> delete(@AuthenticationPrincipal User user,
                                    @RequestParam String bucket,
                                    @RequestParam String objectKey) {
        try {
            String filePath = bucket + "/" + objectKey;

            // 🚨 Check quyền
            if (!securityService.hasPermission(user, PermissionType.FULL, filePath)) {
                return ResponseEntity.status(403).body("Bạn không có quyền xóa: " + filePath);
            }

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
