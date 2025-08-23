package com.company.minio2.view.minio;

import com.company.minio2.dto.BucketDto;
import com.company.minio2.dto.ObjectDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.service.minio.IBucketService;
import com.company.minio2.service.minio.IFileService;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.grid.ItemDoubleClickEvent;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
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

    @ViewComponent("backBtn")
    private JmixButton backBtn;

    @ViewComponent
    private DataGrid<ObjectDto> objects;

    @ViewComponent
    private TypedTextField<String> prefixField;
    @ViewComponent
    private String currentBucket;
    @ViewComponent
    private String currentPrefix = "";

    @Subscribe
    public void onInit(InitEvent event) {
        loadAllBuckets();
        viewItemFile();
        viewItemObject();
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
        }else if (selected != null && TreeNode.FOLDER.equals(selected.getType())) {
            return selected.getParent().getBucketName();
        }
        return null;
    }
//    @Subscribe
//    private String getSelectedForderName() {
//        ObjectDto selected = objects == null ? null : objects.getSingleSelectedItem();
//        if (selected != null && TreeNode.FOLDER.equals(selected.getType())) {
//            return selected.getName();
//        }
//        return null;
//    }

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

            // Prefix là path của folder hiện tại
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

    //icon-css
    private void viewItemObject() {
        if (buckets.getColumnByKey("name") != null) {
            buckets.removeColumn(buckets.getColumnByKey("name"));
        }
        TreeDataGrid.Column<BucketDto> nameColumn = buckets.addComponentHierarchyColumn(this::createBucketItem);
        nameColumn.setHeader("Bucket");
        buckets.setColumnPosition(nameColumn, 0);
    }

    private void viewItemFile() {
        if (objects.getColumnByKey("name") != null) {
            objects.removeColumn(objects.getColumnByKey("name"));
            DataGrid.Column<ObjectDto> nameColumn = objects.addComponentColumn(this::createObjectItem);
            nameColumn.setHeader("File");
            objects.setColumnPosition(nameColumn, 0);
        }
    }

    //icon css bucket
    private HorizontalLayout createBucketItem(BucketDto item) {
        HorizontalLayout layout = new HorizontalLayout();
        layout.setAlignItems(FlexComponent.Alignment.CENTER);
        layout.setPadding(false);
        layout.setSpacing(true);

        Icon icon;
        if (item.getType() == TreeNode.BUCKET) {
            icon = VaadinIcon.ARCHIVE.create();
            icon.addClassName("bucket-item");
        } else if (item.getType() == TreeNode.FOLDER) {
            icon = VaadinIcon.FOLDER.create();
            icon.addClassName("folder-item");
        } else if (item.getType() == TreeNode.FILE) {
            icon = VaadinIcon.FILE.create();
            icon.addClassName("file-item");
        } else {
            icon = VaadinIcon.FILE_O.create();
            icon.addClassName("file-item");
        }

        icon.getElement().getStyle().set("flex-shrink", "0");

        Span text = new Span(item.getBucketName() != null ? item.getBucketName() : "");
        layout.add(icon, text);

        return layout;
    }

    //icon css object
    private HorizontalLayout createObjectItem(ObjectDto item) {
        HorizontalLayout layout = new HorizontalLayout();
        layout.setAlignItems(FlexComponent.Alignment.CENTER);
        layout.setPadding(false);
        layout.setSpacing(true);

        Icon icon = new Icon();
        TreeNode type = item.getType();

        if (type == TreeNode.FOLDER) {
            icon = VaadinIcon.FOLDER.create();
            icon.addClassName("folder-item");
        } else if (type == TreeNode.FILE) {
            icon = VaadinIcon.FILE.create();
            icon.addClassName("file-item");
        } else if (type == TreeNode.BUCKET) {
            icon = VaadinIcon.ARCHIVE.create();
            icon.addClassName("bucket-item");
        }

        icon.getElement().getStyle().set("flex-shrink", "0");

        Span text = new Span(item.getName() != null ? item.getName() : "");
        layout.add(icon, text);

        return layout;
    }



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

    //sự kiện click-2 sẽ mở object con
    @Subscribe("objects")
    public void onObjectsItemDoubleClick(final ItemDoubleClickEvent<ObjectDto> event) {
        ObjectDto item = event.getItem();
        if (item == null) return;

        if (TreeNode.FOLDER.equals(item.getType())) {
            String bucketName = getSelectedBucketName();
            if (bucketName == null) {
                Notification.show("Không tìm thấy bucket");
                return;
            }
            try {
                if (item.getChildren() == null || item.getChildren().isEmpty()) {
                    List<ObjectDto> children = fileService.openFolder(bucketName, item.getKey() == null ? item.getName() + "/" : item.getKey());
                    item.setChildren(children);
                }
                filesDc.setItems(item.getChildren());
            } catch (Exception e) {
                Notification.show("Load folder con thất bại: " + e.getMessage());
            }
        }
        else if (TreeNode.FILE.equals(item.getType())) {
            Notification.show("Double click file: " + item.getName());
        }
    }
    @Subscribe("backBtn")
    public void onBackBtnClick(ClickEvent<JmixButton> event) {
        BucketDto selected = buckets.getSingleSelectedItem();
        String currentBucket = selected == null ? null : selected.getBucketName();
        ObjectDto selectedItem = objects.getSingleSelectedItem();
        String currentPrefix = selectedItem == null ? null : selectedItem.getPath();
        if (currentBucket == null || currentBucket.isBlank()) {
            Notification.show("Chưa chọn bucket.");
            return;
        }
        if (currentPrefix == null || currentPrefix.isBlank()) {
            Notification.show("Đang ở root.");
            return;
        }
        try {

            List<ObjectDto> parentList = fileService.back(currentBucket, currentPrefix);
            filesDc.setItems(parentList);

            // cập nhật lại prefix hiện tại thành cha
            currentPrefix = fileService.parentPrefix(currentPrefix);

            backBtn.setEnabled(currentPrefix != null && !currentPrefix.isBlank());

            Notification.show(currentPrefix.isEmpty() ? "Đã về root" : "Back: " + currentPrefix);
        } catch (Exception ex) {
            Notification.show("Lỗi quay lại: " + ex.getMessage());
        }
    }
}