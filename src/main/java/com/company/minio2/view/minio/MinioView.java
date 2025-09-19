package com.company.minio2.view.minio;

import com.company.minio2.dto.BucketDto;
import com.company.minio2.dto.ObjectDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.exception.MinioException;
import com.company.minio2.service.minio.IBucketService;
import com.company.minio2.service.minio.IFileService;
import com.company.minio2.view.main.MainView;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.confirmdialog.ConfirmDialog;
import com.vaadin.flow.component.grid.ItemClickEvent;
import com.vaadin.flow.component.grid.ItemDoubleClickEvent;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.router.Route;

import io.jmix.flowui.Dialogs;
import io.jmix.flowui.Notifications;
import io.jmix.flowui.app.inputdialog.DialogActions;
import io.jmix.flowui.app.inputdialog.DialogOutcome;

import io.jmix.flowui.component.grid.DataGrid;
import io.jmix.flowui.component.grid.TreeDataGrid;
import io.jmix.flowui.component.textfield.TypedTextField;
import com.vaadin.flow.component.upload.receivers.MultiFileMemoryBuffer;
import io.jmix.flowui.kit.action.ActionPerformedEvent;
import io.jmix.flowui.kit.component.button.JmixButton;


import io.jmix.flowui.model.CollectionContainer;
import io.jmix.flowui.view.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.InputStream;
import java.net.URLConnection;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import static io.jmix.flowui.app.inputdialog.InputParameter.stringParameter;

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
    private CollectionContainer<ObjectDto> objectDc;

    @ViewComponent
    private CollectionContainer<ObjectDto> metadataDc;

    @ViewComponent
    private TreeDataGrid<BucketDto> buckets;

    @ViewComponent("backBtn")
    private JmixButton backBtn;

    @ViewComponent
    private DataGrid<ObjectDto> objects;

    @ViewComponent("metadataPanel")
    private VerticalLayout metadataPanel;

    @ViewComponent("metadataContent")
    private VerticalLayout metadataContent;

    @ViewComponent("metadataName")
    private Span metadataName;

    @ViewComponent("metadataSize")
    private Span metadataSize;

    @ViewComponent("metadataLastModified")
    private Span metadataLastModified;

    @ViewComponent("metadataETag")
    private Span metadataETag;

    @ViewComponent("metadataContentType")
    private Span metadataContentType;

    @ViewComponent("toggleMetadataBtn")
    private JmixButton toggleMetadataBtn;

    @ViewComponent
    private TypedTextField<String> prefixField;

    private String currentBucket;
    private String currentPrefix = "";
    private boolean metadataVisible = true;

    @ViewComponent("fileUpload")
    private Upload fileUpload;
    private MultiFileMemoryBuffer fileBuffer;
    @Autowired
    private Dialogs dialogs;
    @Autowired
    private Notifications notifications;
    @Subscribe
    public void onInit(InitEvent event) {
        loadAllBuckets();
        viewItemFile();
        viewItemObject();

        metadataPanel.setVisible(false);
        metadataVisible = false;
        toggleMetadataBtn.setText("Show");
        clearMetadata();

        //upload file - oninit
        fileBuffer = new MultiFileMemoryBuffer();
        fileUpload.getElement().setProperty("directory", true);
        fileUpload.setDropAllowed(true);
        fileUpload.setReceiver(fileBuffer);
        fileUpload.setMaxFileSize(100 * 1024 * 1024); // 100MB mỗi file
        fileUpload.setMaxFiles(Integer.MAX_VALUE);
        fileUpload.setUploadButton(new Button("Upload File"));
        fileUpload.addSucceededListener(e -> {
            try (InputStream in = fileBuffer.getInputStream(e.getFileName())) {
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
        fileUpload.addAllFinishedListener(e -> {
            refreshFiles();
            refreshBuckets();
            fileUpload.clearFileList();
            fileBuffer = new MultiFileMemoryBuffer();
            fileUpload.setReceiver(fileBuffer);
        });
    }

    private static String buildObjectKey(String prefix, String fileName) {
        if (prefix == null || prefix.isBlank()) return fileName;
        return prefix.endsWith("/") ? prefix + fileName : prefix + "/" + fileName;
    }


    //load all bucket - minio
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
                    });
            bucketsDc.setItems(list);
            buckets.addSelectionListener(e -> loadObjectFromBucket());
        } catch (Exception e) {
           notifications.show("Load buckets failed: " + e.getMessage());
        }
    }


    //load all object theo bucket name
    private void loadObjectFromBucket() {
        try {
            BucketDto selected = buckets.getSingleSelectedItem();
            if (selected == null) {
                updateState(null, "");
                objectDc.setItems(List.of());
                return;
            }
            BucketDto root = rootOf(selected);
            String bucket = root != null ? root.getBucketName() : null;
            String prefix = TreeNode.FOLDER.equals(selected.getType()) ? selected.getPath() : "";
            updateState(bucket, prefix);
            refreshFiles();
        } catch (Exception e) {
            notifications.show("Load object failed: " + e.getMessage());
        }
    }
    private void loadFileMetadata(ObjectDto file) {
        if (currentBucket == null || currentBucket.isBlank()) {
            notifications.show("Chọn bucket trước");
            return;
        }
        if (file == null || file.getKey() == null || file.getKey().isBlank()) {
            notifications.show("Không có thông tin file");
            return;
        }
        try {
            ObjectDto fileMetadata = fileService.getObjectDetail(currentBucket, file.getKey());
            metadataName.setText(fileMetadata.getName() != null ? fileMetadata.getName() : "N/A");
            metadataSize.setText(fileMetadata.getSize() != null ? formatFileSize(fileMetadata.getSize()) : "N/A");
            metadataLastModified.setText(fileMetadata.getLastModified() != null ? 
                fileMetadata.getLastModified().toString() : "N/A");
            
            // Parse metadata string to extract ETag and Content-Type
            String metadataString = fileMetadata.getPath();
            String etag = "N/A";
            String contentType = "N/A";
            
            if (metadataString != null && !metadataString.isEmpty()) {
                String[] parts = metadataString.split(" \\| ");
                for (String part : parts) {
                    if (part.startsWith("ETag: ")) {
                        etag = part.substring(6);
                    } else if (part.startsWith("Content-Type: ")) {
                        contentType = part.substring(14);
                    }
                }
            }
            
            metadataETag.setText(etag);
            metadataContentType.setText(contentType);
            
            // Show the metadata panel if it's hidden
            if (!metadataVisible) {
                metadataVisible = true;
                metadataPanel.setVisible(true);
                toggleMetadataBtn.setText("Hide");
            }
        } catch (Exception e) {
            notifications.show("Không thể load metadata: " + e.getMessage());
        }
    }
    
    private String formatFileSize(Long size) {
        if (size == null) return "N/A";
        if (size < 1024) return size + " B";
        if (size < 1024 * 1024) return String.format("%.1f KB", size / 1024.0);
        if (size < 1024 * 1024 * 1024) return String.format("%.1f MB", size / (1024.0 * 1024.0));
        return String.format("%.1f GB", size / (1024.0 * 1024.0 * 1024.0));
    }

    // làm mới object - data
    private void refreshFiles() {
        try {
            if (currentBucket == null || currentBucket.isBlank()) {
                objectDc.setItems(List.of());
                if (backBtn != null) backBtn.setEnabled(false);
                return;
            }
            List<ObjectDto> list = fileService.getAllFromBucket(currentBucket, currentPrefix);
            objectDc.setItems(list);
        } catch (Exception e) {
            notifications.show("Load object failed: " + e.getMessage());
        }
    }

    // làm mới bucket - tree data
    private void refreshBuckets() {
        try {
            List<BucketDto> list = bucketService.listBucketFolderTree();
            bucketsDc.setItems(list);
        } catch (Exception e) {
            notifications.show("Load buckets failed: " + e.getMessage());
        }

    }

    //tạo bucket
    @Subscribe(id = "createBucketBtn", subject = "clickListener")
    public void onCreateBucketBtnClick(final ClickEvent<JmixButton> event) {
        dialogs.createInputDialog(this)
                .withHeader("Nhập tên bucket")
                .withParameter(stringParameter("name").withLabel("Tên bucket").withRequired(true).withDefaultValue("new-bucket"))
                .withActions(DialogActions.OK_CANCEL)
                .withCloseListener(closeEvent -> {
                    if (closeEvent.closedWith(DialogOutcome.OK)) {
                        String name = closeEvent.getValue("name");
                        if (name.isBlank()) {
                            notifications.show("Tên bucket không được để trống!");
                            return;
                        }
                        try {
                            bucketService.createBucket(name.toLowerCase(Locale.ROOT));
                            notifications.show("Tạo bucket " + name + " thành công!");
                            refreshBuckets();
                        } catch (Exception e) {
                            notifications.show("Lỗi không thể tạo bucket" + name);
                        }
                    }
                })
                .open();
    }

    //tìm kiếm theo prefix
    @Subscribe(id = "searchBtn", subject = "clickListener")
    public void onSearchBtnClick(final ClickEvent<JmixButton> event) {
        if (currentBucket == null || currentBucket.isBlank()) {
            Notification.show("Chưa chọn bucket");
            return;
        }
        String prefix = prefixField != null ? prefixField.getValue() : null;
        String nameFragment = (prefixField != null) ? prefixField.getValue() : "";


        try {
            List<ObjectDto> results = fileService.search(currentBucket, prefix, nameFragment);
            objectDc.setItems(results);
            updateState(currentBucket, prefix);
            Notification.show("Tìm thấy " + results.size() + " object");
        } catch (Exception e) {
           notifications.show("Search failed: " + e.getMessage());
        }

        updateState(currentBucket, prefixField != null ? prefixField.getValue() : currentPrefix);
        refreshFiles();
    }

    //toggle metadata panel
    @Subscribe(id = "toggleMetadataBtn", subject = "clickListener")
    public void onToggleMetadataBtnClick(final ClickEvent<JmixButton> event) {
        metadataVisible = !metadataVisible;
        if (metadataVisible) {
            metadataPanel.setVisible(true);
            toggleMetadataBtn.setText("Hide");
        } else {
            metadataPanel.setVisible(false);
            toggleMetadataBtn.setText("Show");
        }
    }

    // back to file
    @Subscribe("backBtn")
    public void onBackBtnClick(ClickEvent<JmixButton> event) {
        if (currentBucket == null || currentBucket.isBlank()) {
            Notification.show("Chưa chọn bucket.");
            return;
        }
        if (currentPrefix == null || currentPrefix.isBlank()) {
            //Notification.show("Đang ở root.");
            return;
        }
        try {
            List<ObjectDto> parentList = fileService.back(currentBucket, currentPrefix);
            objectDc.setItems(parentList);
            String newPrefix = fileService.parentPrefix(currentPrefix);
            updateState(currentBucket, newPrefix);
            //Notification.show(currentPrefix.isEmpty() ? "Đã về root" : "Back: " + currentPrefix);
        } catch (Exception ex) {
            notifications.show("Back failed : " + ex.getMessage());
        }
    }

    // 2 click: mở object con bên phải
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
                objectDc.setItems(item.getChildren());
            } catch (Exception e) {
                notifications.show("Load folder con thất bại" + e);
            }
        } else if (TreeNode.FILE.equals(item.getType())) {
            // Show metadata for the selected file
            loadFileMetadata(item);
        }
    }



    @Subscribe("buckets")
    public void onBucketsItemClick(final ItemClickEvent<BucketDto> event) {
        BucketDto item = event.getItem();
        if (item == null) return;

        if (TreeNode.BUCKET.equals(item.getType())) {
            updateState(item.getBucketName(), "");
//            Notification.show("name: " + this.currentBucket);
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

    @Subscribe("objects")
    public void onObjectsItemClick(final ItemClickEvent<ObjectDto> event) {
        ObjectDto item = event.getItem();
        if (item == null) return;
        
        // Refresh metadata when clicking on any file or folder
        if (TreeNode.FILE.equals(item.getType())) {
            loadFileMetadata(item);
        } else if (TreeNode.FOLDER.equals(item.getType())) {
            // Clear metadata for folders
            clearMetadata();
        }
    }
    
    private void clearMetadata() {
        metadataName.setText("N/A");
        metadataSize.setText("N/A");
        metadataLastModified.setText("N/A");
        metadataETag.setText("N/A");
        metadataContentType.setText("N/A");
    }

    //menu tạo mới folder - data
    @Subscribe("objects.create")
    public void onObjectsCreate(final ActionPerformedEvent event) {
        if (currentBucket == null || currentBucket.isBlank()) {
            notifications.show("Vui lòng chọn bucket trước!");
            return;
        }
        dialogs.createInputDialog(this)
                .withHeader("Tạo mới folder")
                .withParameters(
                        stringParameter("name")
                                .withLabel("Tên Folder ")
                                .withRequired(true)
                                .withDefaultValue("New Folder")
                )
                .withActions(DialogActions.OK_CANCEL)
                .withCloseListener(closeEvent -> {
                    if (closeEvent.closedWith(DialogOutcome.OK)) {
                        try {
                            String objectKey = closeEvent.getValue("name");
                            fileService.createNewObject(currentBucket, currentPrefix, objectKey);
                            refreshBuckets();
                            refreshFiles();
                            notifications.show("Tạo mới folder thành công");
                        } catch (MinioException e) {
                            notifications.show("Không thể tạo mới folder" + e);
                        }
                    }
                })
                .open();
    }


    //action xóa file - data
    @Subscribe("objects.delete")
    public void onObjectsDelete(final ActionPerformedEvent event) {
        ObjectDto object = objects.getSingleSelectedItem();
        if (currentBucket == null || currentBucket.isBlank()) {
            Notification.show("Chưa chọn bucket");
            return;
        }
        if (object == null || object.getKey() == null || object.getKey().isBlank()) {
            Notification.show("Chọn folder or file để xóa!");
            return;
        }
        ConfirmDialog dlg = new ConfirmDialog();
        dlg.setHeader("Xác nhận");
        dlg.setText("Xóa file '" + object.getName() + " ?");
        dlg.setCancelable(true);
        dlg.setConfirmText("Xóa");
        dlg.addConfirmListener(e2 -> {
            try {
                fileService.delete(currentBucket, object.getKey());
                Notification.show("Đã xóa file: " + object.getName());
                refreshFiles();
                refreshBuckets();
            } catch (Exception ex) {
                notifications.show("Không thể xóa bucket (có thể bucket chưa rỗng)." + ex);
            }
        });
        dlg.open();
    }


    //action download file - data
    @Subscribe("objects.download")
    public void onObjectsDownload(final ActionPerformedEvent event) {
        ObjectDto selected = objects.getSingleSelectedItem();
        if (selected == null || selected.getKey() == null || selected.getKey().isBlank()) {
            Notification.show("Chưa chọn file");
            return;
        }
        if (currentBucket == null || currentBucket.isBlank()) {
            Notification.show("Chưa chọn bucket");
            return;
        }
        try {
            String url = fileService.download(currentBucket, selected.getKey(), 300);
            getUI().ifPresent(ui -> ui.getPage().open(url));
            Notification.show("Downloading '" + selected.getKey() + "'");
        } catch (Exception e) {
            Notification.show("Tải xuống thất bại: " + e.getMessage());
        }
    }


    //action upload file - data
    @Subscribe("objects.upload")
    public void onObjectsUpload(ActionPerformedEvent event) {

    }

    //action tạo mới bucket
    @Subscribe("buckets.create")
    public void onBucketsCreate(final ActionPerformedEvent event) {
        dialogs.createInputDialog(this)
                .withHeader("Nhập tên bucket")
                .withParameter(stringParameter("name").withLabel("Tên bucket").withRequired(true).withDefaultValue("new-bucket"))
                .withActions(DialogActions.OK_CANCEL)
                .withCloseListener(closeEvent -> {
                    if (closeEvent.closedWith(DialogOutcome.OK)) {
                        String name = closeEvent.getValue("name");
                        if (name.isBlank()) {
                            notifications.show("Tên bucket không được để trống!");
                            return;
                        }
                        try {
                            bucketService.createBucket(name.toLowerCase(Locale.ROOT));
                            notifications.show("Tạo bucket " + name + " thành công!");
                            refreshBuckets();
                        } catch (Exception e) {
                            notifications.show("Lỗi không thể tạo bucket" + name);
                        }
                    }
                })
                .open();
    }

    //action xóa bucket và folder - tree
    @Subscribe("buckets.delete")
    public void onBucketsDelete(final ActionPerformedEvent event) {
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
                dlg.setText("Xóa bucket '" + selected.getBucketName() + " ?");
                dlg.setCancelable(true);
                dlg.setConfirmText("Xóa");
                dlg.addConfirmListener(e2 -> {
                    try {
                        bucketService.removeBucket(currentBucket);
                        Notification.show("Đã xóa bucket: " + currentBucket);
                        refreshBuckets();
                        refreshFiles();
                    } catch (Exception ex) {
                       notifications.show("Không thể xóa bucket (có thể bucket chưa rỗng)" + ex);
                    }
                });
                dlg.open();

            } else if (TreeNode.FOLDER.equals(selected.getType())) {
                ConfirmDialog dlg = new ConfirmDialog();
                dlg.setHeader("Xác nhận");
                dlg.setText("Xóa FOLDER '" + currentPrefix + "' và toàn bộ bên trong?");
                dlg.setCancelable(true);
                dlg.setConfirmText("Xóa");
                dlg.addConfirmListener(e2 -> {
                    try {
                        fileService.delete(currentBucket, currentPrefix);
                        Notification.show("Đã xóa folder: " + currentPrefix);
                        refreshBuckets();
                        refreshFiles();
                    } catch (Exception ex) {
                       notifications.show("Không thể xóa folder" + ex);
                    }
                });
                dlg.open();
            } else {
                Notification.show("Vui lòng chọn Bucket hoặc Folder.");
            }
        } catch (Exception e) {
            notifications.show("Không thể xóa" + e);
        }
    }
    // action tạo mới folder - tree
    @Subscribe("buckets.createFolder")
    public void onBucketsCreateFolder(final ActionPerformedEvent event) {
        if (currentBucket == null || currentBucket.isBlank()) {
            notifications.show("Vui lòng chọn bucket trước!");
            return;
        }
        dialogs.createInputDialog(this)
                .withHeader("Tạo mới folder")
                .withParameters(
                        stringParameter("name")
                                .withLabel("Tên Folder ")
                                .withRequired(true)
                                .withDefaultValue("New Folder")
                )
                .withActions(DialogActions.OK_CANCEL)
                .withCloseListener(closeEvent -> {
                    if (closeEvent.closedWith(DialogOutcome.OK)) {
                        try {
                            String objectKey = closeEvent.getValue("name");
                            fileService.createNewObject(currentBucket, currentPrefix, objectKey);
                            refreshBuckets();
                            refreshFiles();
                            notifications.show("Tạo mới folder thành công");
                        } catch (MinioException e) {
                            notifications.show("Không thể tạo mới folder" + e);
                        }
                    }
                })
                .open();
    }



}
