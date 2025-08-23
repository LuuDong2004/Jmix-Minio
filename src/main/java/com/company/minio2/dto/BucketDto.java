package com.company.minio2.dto;

import io.jmix.core.entity.annotation.JmixId;
import io.jmix.core.metamodel.annotation.JmixEntity;
import io.jmix.core.metamodel.annotation.JmixProperty;

import java.util.List;

@JmixEntity(name = "minio_bucket")
public class BucketDto {
    @JmixId
    private String id; //id của từng bucket hoặc class folder

    private List<BucketDto> children;

    @JmixProperty(mandatory = true)
    private String bucketName; // name của bucket


    @JmixProperty(mandatory = true)
    private String creatDate; // ngày tạo

    @JmixProperty(mandatory = true)
    private String path;

    private String type;

    private BucketDto parent;

    public List<BucketDto> getChildren() {
        return children;
    }

    public void setChildren(List<BucketDto> children) {
        this.children = children;
    }

    public void setType(TreeNode type) {
        this.type = type == null ? null : type.getId();
    }

    public TreeNode getType() {
        return type == null ? null : TreeNode.fromId(type);
    }


    public BucketDto() {
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

    public BucketDto getParent() {
        return parent;
    }

    public void setParent(BucketDto parent) {
        this.parent = parent;
    }


}
