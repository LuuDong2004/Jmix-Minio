package com.company.minio2.view.minio;

import com.company.minio2.dto.BucketDto;
import com.company.minio2.dto.ObjectDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.service.minio.IBucketService;
import com.company.minio2.service.minio.IFileService;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.grid.ItemClickEvent;
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

import java.util.ArrayDeque;
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


    private String currentBucket;
    private String currentPrefix = "";


    @Subscribe
    public void onInit(InitEvent event) {
        loadAllBuckets();
        viewItemFile();
        viewItemObject();
    }
    private void loadAllBuckets() {
        try {
            List<BucketDto> list = bucketService.listBucketFolderTree();

            // auto select bucket đầu tiên (nếu có)
            list.stream()
                    .filter(b -> TreeNode.BUCKET.equals(b.getType()))
                    .findFirst()
                    .ifPresent(first -> {
                        buckets.select(first);
                        updateState(first.getBucketName(), "");
                        refreshFiles();
                        selectTreeNode(currentBucket, currentPrefix);
                    });
            bucketsDc.setItems(list);


            buckets.addSelectionListener(e -> loadObjectFromBucket());
        } catch (Exception e) {
            toastErr("Load buckets failed", e);
        }
    }

    private void loadObjectFromBucket() {
        try {
            BucketDto selected = buckets.getSingleSelectedItem();
            if (selected == null) {
                updateState(null, "");
                filesDc.setItems(List.of());
                return;
            }
            BucketDto root = rootOf(selected);
            String bucket = root != null ? root.getBucketName() : null;
            String prefix = TreeNode.FOLDER.equals(selected.getType()) ? selected.getPath() : "";
            updateState(bucket, prefix);
            refreshFiles();
        } catch (Exception e) {
            toastErr("Load error", e);
        }
    }

    private void refreshFiles() {
        try {
            if (currentBucket == null || currentBucket.isBlank()) {
                filesDc.setItems(List.of());
                if (backBtn != null) backBtn.setEnabled(false);
                return;
            }
            List<ObjectDto> list = fileService.getAllFromBucket(currentBucket, currentPrefix);
            filesDc.setItems(list);
        } catch (Exception e) {
            toastErr("Load folder/file thất bại", e);
        }
    }

    // ===== ACTIONS (Buttons) =====
    @Subscribe(id = "createBucketBtn", subject = "clickListener")
    public void onCreateBucketBtnClick(final ClickEvent<JmixButton> event) {
        Notification.show("Chức năng tạo bucket chưa được triển khai");
    }

    @Subscribe(id = "searchBtn", subject = "clickListener")
    public void onSearchBtnClick(final ClickEvent<JmixButton> event) {
        updateState(currentBucket, prefixField != null ? prefixField.getValue() : currentPrefix);
        refreshFiles();
    }

    @Subscribe(id = "downloadBtn", subject = "clickListener")
    public void onDownloadBtnClick(final ClickEvent<JmixButton> event) {
        Notification.show("Chức năng tải xuống chưa được triển khai");
    }

    @Subscribe(id = "deleteBtn", subject = "clickListener")
    public void onDeleteFileBtnClick(final ClickEvent<JmixButton> event) {
        Notification.show("Chức năng xóa file chưa được triển khai");
    }

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

                // nếu đang đứng trong bucket vừa xóa -> reset state
                if (selected.getBucketName() != null && selected.getBucketName().equals(currentBucket)) {
                    updateState(null, "");
                    filesDc.setItems(List.of());
                }
                loadAllBuckets();
            } else {
                Notification.show("Chưa hỗ trợ xóa folder");
            }
        } catch (Exception e) {
            toastErr("Không thể xóa", e);
        }
    }

    @Subscribe("backBtn")
    public void onBackBtnClick(ClickEvent<JmixButton> event) {
        if (currentBucket == null || currentBucket.isBlank()) {
            Notification.show("Chưa chọn bucket."); return;
        }
        if (currentPrefix == null || currentPrefix.isBlank()) {
            Notification.show("Đang ở root."); return;
        }
        try {
            List<ObjectDto> parentList = fileService.back(currentBucket, currentPrefix);
            filesDc.setItems(parentList);

            // cập nhật prefix về cha
            String newPrefix = fileService.parentPrefix(currentPrefix);
            updateState(currentBucket, newPrefix);
            selectTreeNode(currentBucket, currentPrefix);

            Notification.show(currentPrefix.isEmpty() ? "Đã về root" : "Back: " + currentPrefix);
        } catch (Exception ex) {
            toastErr("Lỗi quay lại", ex);
        }
    }

    // ===== GRID EVENTS =====
    // Double click: mở object con bên phải
    @Subscribe("objects")
    public void onObjectsItemDoubleClick(final ItemDoubleClickEvent<ObjectDto> event) {
        ObjectDto item = event.getItem();
        if (item == null) return;

        if (TreeNode.FOLDER.equals(item.getType())) {
            if (currentBucket == null || currentBucket.isBlank()) {
                Notification.show("Không tìm thấy bucket");
                return;
            }
            try {
                // Ưu tiên key (full path), fallback path/name
                String next = (item.getKey() != null && !item.getKey().isBlank())
                        ? item.getKey()
                        : (item.getPath() != null && !item.getPath().isBlank())
                        ? item.getPath()
                        : item.getName();
                updateState(currentBucket, next);

                // load children (tận dụng openFolder nếu muốn cache)
                if (item.getChildren() == null || item.getChildren().isEmpty()) {
                    List<ObjectDto> children = fileService.openFolder(currentBucket, currentPrefix);
                    item.setChildren(children);
                }
                filesDc.setItems(item.getChildren());
                selectTreeNode(currentBucket, currentPrefix);
            } catch (Exception e) {
                toastErr("Load folder con thất bại", e);
            }
        } else if (TreeNode.FILE.equals(item.getType())) {
            Notification.show("Double click file: " + item.getName());
        }
    }

    // Click vào 1 item của tree (buckets)
    @Subscribe("buckets")
    public void onBucketsItemClick(final ItemClickEvent<BucketDto> event) {
        BucketDto item = event.getItem();
        if (item == null) return;

        if (TreeNode.BUCKET.equals(item.getType())) {
            updateState(item.getBucketName(), "");
            Notification.show("name: " + this.currentBucket);
            refreshFiles();
        }
        selectTreeNode(currentBucket, currentPrefix);
    }


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

    // ===== LEGACY COMPAT (giữ tên nếu code khác còn gọi) =====
    // Dùng để đồng bộ state theo selection hiện tại (nếu framework gọi).
    @Subscribe
    private String getSelectedBucketName() {
        BucketDto selected = buckets == null ? null : buckets.getSingleSelectedItem();
        if (selected == null) {
            updateState(null, "");
            return null;
        }
        BucketDto root = rootOf(selected);
        updateState(root != null ? root.getBucketName() : null,
                TreeNode.FOLDER.equals(selected.getType()) ? selected.getPath() : "");
        return currentBucket;
    }

    private static String norm(String prefix) {
        if (prefix == null || prefix.isBlank()) return "";
        return prefix.endsWith("/") ? prefix : prefix + "/";
    }

    // luôn lấy bucket gốc của 1 node
    private static BucketDto rootOf(BucketDto n) {
        while (n != null && n.getParent() != null) n = n.getParent();
        return n;
    }

    // cập nhật state + đồng bộ UI
    private void updateState(String bucket, String prefix) {
        this.currentBucket = bucket;
        this.currentPrefix = norm(prefix);
        if (prefixField != null) prefixField.setValue(this.currentPrefix);
        if (backBtn != null) backBtn.setEnabled(!this.currentPrefix.isBlank());
    }

    private static void toastErr(String msg, Exception e) {
        Notification.show(msg + ": " + (e.getMessage() == null ? e.toString() : e.getMessage()));
    }


    @SuppressWarnings("unused")
    private void selectTreeNode(String bucket, String prefix) {
        if (bucketsDc == null || bucketsDc.getItems() == null) return;
        String p = norm(prefix);
        ArrayDeque<BucketDto> q = new ArrayDeque<>(bucketsDc.getItems());
        while (!q.isEmpty()) {
            BucketDto n = q.remove();
            boolean ok =
                    (TreeNode.BUCKET.equals(n.getType()) && p.isBlank()
                            && bucket != null && bucket.equals(n.getBucketName()))
                            || (TreeNode.FOLDER.equals(n.getType())
                            && bucket != null && bucket.equals(n.getBucketName())
                            && p.equals(norm(n.getPath())));
            if (ok) {
                buckets.select(n);
                try { buckets.expand(n); } catch (Throwable ignore) {}
                return;
            }
            if (n.getChildren() != null) q.addAll(n.getChildren());
        }
    }
}
