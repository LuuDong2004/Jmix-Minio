package com.company.minio2.dto;

import io.jmix.core.entity.annotation.JmixId;
import io.jmix.core.metamodel.annotation.JmixEntity;
import io.jmix.core.metamodel.annotation.JmixProperty;

@JmixEntity(name="minio_bucket")
public class BucketDto {
    @JmixId
    private String id; //id của từng bucket hoặc class folder

    @JmixProperty(mandatory = true)
    private String bucketName; // name của bucket


    @JmixProperty(mandatory = true)
    private String creatDate; // ngày tạo

    @JmixProperty(mandatory = true)
    private String path;

    @JmixProperty()
    private TreeNode type;

    @JmixProperty()
    private BucketDto parent;

    public BucketDto() {
    }
    public BucketDto(String id, String bucketName, String creatDate, String path) {
        this.id = id;
        this.bucketName = bucketName;
        this.creatDate = creatDate;
        this.path = path;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getBucketName() {
        return bucketName;
    }

    public void setBucketName(String bucketName) {
        this.bucketName = bucketName;
    }

    public String getCreatDate() {
        return creatDate;
    }

    public void setCreatDate(String creatDate) {
        this.creatDate = creatDate;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public TreeNode getType() {
        return type;
    }

    public void setType(TreeNode type) {
        this.type = type;
    }

    public BucketDto getParent() {
        return parent;
    }

    public void setParent(BucketDto parent) {
        this.parent = parent;
    }
}
