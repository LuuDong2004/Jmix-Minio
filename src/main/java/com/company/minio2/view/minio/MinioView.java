package com.company.minio2.view.minio;

import com.company.minio2.dto.BucketDto;
import com.company.minio2.dto.ObjectDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.service.minio.IBucketService;
import com.company.minio2.service.minio.IFileService;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.FocusNotifier;
import com.vaadin.flow.component.grid.ItemClickEvent;
import com.vaadin.flow.component.notification.Notification;

import com.vaadin.flow.component.treegrid.ExpandEvent;
import com.vaadin.flow.router.Route;
import io.jmix.flowui.component.grid.DataGrid;
import io.jmix.flowui.component.grid.TreeDataGrid;
import io.jmix.flowui.component.textfield.TypedTextField;
import io.jmix.flowui.kit.component.button.JmixButton;
import io.jmix.flowui.model.CollectionContainer;
import io.jmix.flowui.view.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.Set;

@Route(value = "minio-view", layout = MainView.class)
@ViewController(id = "MinioView")
@ViewDescriptor(path = "minio-view.xml")
public class MinioView extends StandardView {
    @Autowired
    private IBucketService bucketService;
    @Autowired
    private IFileService fileService;

    @ViewComponent
    private CollectionContainer<BucketDto> bucketsDc;

    @ViewComponent
    private CollectionContainer<ObjectDto> filesDc;

    @ViewComponent
    private TreeDataGrid<BucketDto> buckets;


    @ViewComponent
    private DataGrid<ObjectDto> object;

    @ViewComponent
    private TypedTextField<String> prefixField;


    // điều hướng phân cấp
    @ViewComponent
    private String currentBucket;
    @ViewComponent
    private String currentPrefix = "";

    @Subscribe
    public void onInit(InitEvent event) {
        loadAllBuckets();
    }
    // hiển thị tất cả bucket và auto hiển thị danh sách Object trong bucket
    private void loadAllBuckets() {
        try {
            List<BucketDto> list = bucketService.listBucketFolderTree();
            list.stream()
                    .filter(b -> TreeNode.BUCKET.equals(b.getType()))
                    .findFirst()
                    .ifPresent(first -> {
                        buckets.select(first);
                        loadAllFilesFromBucket();
                    });
            bucketsDc.setItems(list);
            buckets.addSelectionListener(e -> loadObjectFromBucket());
        } catch (Exception e) {
            Notification.show("Load buckets failed: " + e.getMessage());
        }
    }

    //load tất cả folder and file
    private void loadAllFilesFromBucket() {
        try {
            String bucketName = getSelectedBucketName();
//            if (bucketName == null || bucketName.isBlank()) {
//                filesDc.setItems(List.of());
//                Notification.show("Chọn bucket để xem các file bên trong");
//                return;
//            }
            String prefix = prefixField.getValue();
            List<ObjectDto> list = fileService.getAllFromBucket(bucketName, prefix == null ? "" : prefix);
            filesDc.setItems(list);
            Notification.show("Danh sách folder and file của bucket: " + bucketName);

        } catch (Exception e) {
            Notification.show("Load folder and file failed: " + e.getMessage());
        }
    }
    // sự kiện lấy name bucket khi user click
    @Subscribe
    private String getSelectedBucketName() {
        BucketDto selected = buckets == null ? null : buckets.getSingleSelectedItem();
        if (selected != null && TreeNode.BUCKET.equals(selected.getType())) {
            return selected.getBucketName();
        }
        return null;
    }


    @Subscribe
    private String getSelectedForderName() {
        ObjectDto selected = object  == null ? null : object.getSingleSelectedItem();
        if (selected != null && TreeNode.FOLDER.equals(selected.getType())) {
            return selected.getName();
        }
        return null;
    }
    // xóa bucket
    @Subscribe(id = "deleteBucketBtn", subject = "clickListener")
    public void onDeleteBucketBtnClick(final ClickEvent<JmixButton> event) {
        try {
            Set<BucketDto> selectedItems = buckets.getSelectedItems();
            if (selectedItems == null || selectedItems.isEmpty()) {
                Notification.show("Vui lòng chọn bucket hoặc folder");
                return;
            }

            BucketDto selected = selectedItems.iterator().next();
            if (selected.getType() == null || TreeNode.BUCKET.equals(selected.getType())) {
                bucketService.removeBucket(selected.getBucketName());
                Notification.show("Đã xóa bucket: " + selected.getBucketName());
                loadAllBuckets();
            } else {
                Notification.show("Chưa hỗ trợ xóa folder");
            }
        } catch (Exception e) {
            Notification.show("Không thể xóa: " + e.getMessage());
        }
    }
    private void loadObjectFromBucket() {
//        try {
//            String bucket = getSelectedBucketName();
//            if (bucket == null || bucket.isBlank()) {
//                filesDc.setItems(List.of());
//                return;
//            }
//            String prefix = prefixField.getValue();
//            List<ObjectDto> list = fileService.getAllFromBucket(bucket, prefix == null ? "" : prefix);
//            filesDc.setItems(list);
//        } catch (Exception e) {
//            Notification.show("Load error: " + e.getMessage());
//        }

            try {
                BucketDto selected = buckets.getSingleSelectedItem();
                if (selected == null) {
                    filesDc.setItems(List.of());
                    return;
                }

                // Luôn tìm bucket gốc
                BucketDto root = selected;
                while (root.getParent() != null) {
                    root = root.getParent();
                }
                String bucketName = root.getBucketName();

                // Prefix là path của folder hiện tại (nếu là folder)
                String prefix = "";
                if (TreeNode.FOLDER.equals(selected.getType())) {
                    prefix = selected.getPath();
                }

                List<ObjectDto> list = fileService.getAllFromBucket(bucketName, prefix);
                filesDc.setItems(list);

            } catch (Exception e) {
                Notification.show("Load error: " + e.getMessage());
            }
        }


//    private void loadObjectFromFolder() {
//        try {
//            String folder = getSelectedForderName();
//            if (folder == null || folder.isBlank()) {
//                filesDc.setItems(List.of());
//                return;
//            }
//            String prefix = prefixField.getValue();
//            List<ObjectDto> list = fileService.listChildren(folder, prefix == null ? "" : prefix);
//            filesDc.setItems(list);
//        } catch (Exception e) {
//            Notification.show("Load error: " + e.getMessage());
//        }
//    }




    @Subscribe(id = "createBucketBtn", subject = "clickListener")
    public void onCreateBucketBtnClick(final ClickEvent<JmixButton> event) {
        Notification.show("Chức năng tạo bucket chưa được triển khai");
    }

    @Subscribe(id = "searchBtn", subject = "clickListener")
    public void onSearchBtnClick(final ClickEvent<JmixButton> event) {
        String prefix = prefixField.getValue();
        Notification.show("Tìm với prefix: " + (prefix == null ? "" : prefix));
    }

    @Subscribe(id = "downloadBtn", subject = "clickListener")
    public void onDownloadBtnClick(final ClickEvent<JmixButton> event) {
        Notification.show("Chức năng tải xuống chưa được triển khai");
    }

    @Subscribe(id = "deleteBtn", subject = "clickListener")
    public void onDeleteFileBtnClick(final ClickEvent<JmixButton> event) {
        Notification.show("Chức năng xóa file chưa được triển khai");
    }
//    //sự kiện load các folder và file
//    @Subscribe("buckets")
//    public void onBucketsItemClick(final ItemClickEvent<BucketDto> event) {
//        object.addSelectionListener(e -> loadObjectFromFolder());
//    }

}