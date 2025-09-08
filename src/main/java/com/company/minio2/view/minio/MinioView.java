package com.company.minio2.view.minio;

import com.company.minio2.dto.BucketDto;
import com.company.minio2.dto.ObjectDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.service.minio.IBucketService;
import com.company.minio2.service.minio.IFileService;
import com.company.minio2.view.main.MainView;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.confirmdialog.ConfirmDialog;
import com.vaadin.flow.component.grid.ItemClickEvent;
import com.vaadin.flow.component.grid.ItemDoubleClickEvent;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.router.Route;
import io.jmix.flowui.Dialogs;
import io.jmix.flowui.Notifications;
import io.jmix.flowui.action.inputdialog.InputDialogAction;
import io.jmix.flowui.app.inputdialog.DialogActions;
import io.jmix.flowui.app.inputdialog.DialogOutcome;
import io.jmix.flowui.app.inputdialog.InputParameter;
import io.jmix.flowui.component.grid.DataGrid;
import io.jmix.flowui.component.grid.TreeDataGrid;
import io.jmix.flowui.component.textfield.TypedTextField;
import com.vaadin.flow.component.upload.receivers.MultiFileMemoryBuffer;
import io.jmix.flowui.kit.component.button.JmixButton;

import io.jmix.flowui.model.CollectionContainer;
import io.jmix.flowui.view.*;

import io.minio.errors.MinioException;
import org.springframework.beans.factory.annotation.Autowired;


import java.io.InputStream;
import java.net.URLConnection;
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

    @ViewComponent("multiUpload")
    private Upload multiUpload;

    private MultiFileMemoryBuffer uploadBuffer;
    @Autowired
    private Dialogs dialogs;
    @Autowired
    private Notifications notifications;

    @Subscribe
    public void onInit(InitEvent event) {
        loadAllBuckets();
        viewItemFile();
        viewItemObject();

        uploadBuffer = new MultiFileMemoryBuffer();
        multiUpload.getElement().setProperty("directory", true);
        multiUpload.setDropAllowed(true);
        multiUpload.setReceiver(uploadBuffer);
        multiUpload.setMaxFileSize(100 * 1024 * 1024); // 100MB mỗi file
        multiUpload.setMaxFiles(Integer.MAX_VALUE);


        multiUpload.addSucceededListener(e -> {
            try (InputStream in = uploadBuffer.getInputStream(e.getFileName())) {
                String fileName = e.getFileName();
                long size = e.getContentLength();
                String contentType = e.getMIMEType();
                if (contentType == null || contentType.isBlank()) {
                    String guess = URLConnection.guessContentTypeFromName(fileName);
                    contentType = (guess != null) ? guess : "application/octet-stream";
                }
                String objectKey = buildObjectKey(currentPrefix, fileName);
                fileService.uploadFile(currentBucket, objectKey, in, size, contentType);
                Notification.show("Upload thành công: " + objectKey);
            } catch (Exception ex) {
                Notification.show("Lỗi upload " + e.getFileName() + ": " + ex.getMessage());
            }
        });
        multiUpload.addAllFinishedListener(e -> {
            refreshFiles();
            multiUpload.clearFileList();
            uploadBuffer = new MultiFileMemoryBuffer();
            multiUpload.setReceiver(uploadBuffer);
        });
    }

    private static String buildObjectKey(String prefix, String fileName) {
        if (prefix == null || prefix.isBlank()) return fileName;
        return prefix.endsWith("/") ? prefix + fileName : prefix + "/" + fileName;
    }


    private void loadAllBuckets() {
        try {
            List<BucketDto> list = bucketService.listBucketFolderTree();
            list.stream()
                    .filter(b -> TreeNode.BUCKET.equals(b.getType()))
                    .findFirst()
                    .ifPresent(first -> {
                        buckets.select(first);
                        updateState(first.getBucketName(), "");
                        refreshFiles();
                        //selectTreeNode(currentBucket, currentPrefix);
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

    private void refreshBuckets() {
        try {
            List<BucketDto> list = bucketService.listBucketFolderTree();
            bucketsDc.setItems(list);
        } catch (Exception e) {
            toastErr("Load buckets failed", e);
        }

    }
    @Subscribe(id = "createBucketBtn", subject = "clickListener")
    public void onCreateBucketBtnClick(final ClickEvent<JmixButton> event) {
        dialogs.createInputDialog(this)
                .withHeader("Nhập tên bucket")
                .withParameter(InputParameter.stringParameter("name").withLabel("Tên bucket").withRequired(true))
                .withActions(DialogActions.OK_CANCEL)
                .withCloseListener(closeEvent -> {
                    if (closeEvent.closedWith(DialogOutcome.OK)) {
                        String name = closeEvent.getValue("name");
                        if(name.isBlank()){
                            notifications.show("Tên bucket không được để trống!");
                            return;
                        }
                        try {
                            bucketService.createBucket(name);
                            notifications.show("Tạo bucket "+ name + " thành công!");
                            refreshBuckets();
                        } catch (Exception e) {
                            notifications.show("Lỗi không thể tạo bucket" + name);
                        }
                    }
                })
                .open();
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

    // xóa file
    @Subscribe(id = "deleteBtn", subject = "clickListener")
    public void onDeleteFileBtnClick(ClickEvent<JmixButton> event) {
        ObjectDto object = objects.getSingleSelectedItem();
        if (currentBucket == null || currentBucket.isBlank()) {
            Notification.show("Chưa chọn bucket");
            return;
        }
        if (object == null || object.getKey() == null || object.getKey().isBlank()) {
            Notification.show("Chọn folder or file để xóa!");
            return;
        }
        try {
            fileService.delete(currentBucket, object.getKey());
            Notification.show("xóa thành công!" + object.getKey());
            refreshFiles();
        } catch (Exception e) {
            Notification.show("Lỗi không thể xóa" + object.getKey() + " của bucket " + currentBucket);
        }
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

            if (TreeNode.BUCKET.equals(selected.getType())) {
                ConfirmDialog dlg = new ConfirmDialog();
                dlg.setHeader("Xác nhận");
                dlg.setText("Xóa BUCKET '" + selected.getBucketName() + "'?\n(Lưu ý: bucket phải rỗng)");
                dlg.setCancelable(true);
                dlg.setConfirmText("Xóa");
                dlg.addConfirmListener(e2 -> {
                    try {
                        bucketService.removeBucket(selected.getBucketName());
                        Notification.show("Đã xóa bucket: " + selected.getBucketName());

                        if (selected.getBucketName() != null && selected.getBucketName().equals(currentBucket)) {
                            updateState(null, "");
                            filesDc.setItems(List.of());
                        }
                        loadAllBuckets();
                    } catch (Exception ex) {
                        toastErr("Không thể xóa bucket (có thể bucket chưa rỗng).", ex);
                    }
                });
                dlg.open();

            } else if (TreeNode.FOLDER.equals(selected.getType())) {
                final String bucket = selected.getBucketName();
                String folderPrefix = selected.getPath();
                if (folderPrefix == null) folderPrefix = "";
                if (!folderPrefix.isEmpty() && !folderPrefix.endsWith("/")) folderPrefix = folderPrefix + "/";

                ConfirmDialog dlg = new ConfirmDialog();
                dlg.setHeader("Xác nhận");
                dlg.setText("Xóa FOLDER '" + folderPrefix + "' và toàn bộ bên trong?");
                dlg.setCancelable(true);
                dlg.setConfirmText("Xóa");
                final String finalFolderPrefix = folderPrefix;
                dlg.addConfirmListener(e2 -> {
                    try {
                        fileService.delete(bucket, finalFolderPrefix);
                        Notification.show("Đã xóa folder: " + finalFolderPrefix);

                        if (bucket != null && bucket.equals(currentBucket)) {
                            filesDc.setItems(fileService.getAllFromBucket(currentBucket, currentPrefix));
                        }
                        loadAllBuckets();
                    } catch (Exception ex) {
                        toastErr("Không thể xóa folder", ex);
                    }
                });
                dlg.open();
            } else {
                Notification.show("Vui lòng chọn Bucket hoặc Folder.");
            }
        } catch (Exception e) {
            toastErr("Không thể xóa", e);
        }
    }

    @Subscribe("backBtn")
    public void onBackBtnClick(ClickEvent<JmixButton> event) {
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

            // cập nhật prefix về cha
            String newPrefix = fileService.parentPrefix(currentPrefix);
            updateState(currentBucket, newPrefix);
            //selectTreeNode(currentBucket, currentPrefix);

            Notification.show(currentPrefix.isEmpty() ? "Đã về root" : "Back: " + currentPrefix);
        } catch (Exception ex) {
            toastErr("Lỗi quay lại", ex);
        }
    }

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
                if (item.getChildren() == null || item.getChildren().isEmpty()) {
                    List<ObjectDto> children = fileService.openFolder(currentBucket, currentPrefix);
                    item.setChildren(children);
                }
                filesDc.setItems(item.getChildren());
                //selectTreeNode(currentBucket, currentPrefix);
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

}
