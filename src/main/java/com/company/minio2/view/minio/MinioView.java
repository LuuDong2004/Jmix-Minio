package com.company.minio2.view.minio;

import com.company.minio2.dto.BucketDto;
import com.company.minio2.dto.FileDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.service.minio.File2Service;
import com.company.minio2.service.minio.FileService;
import com.company.minio2.service.minio.IBucketService;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.button.Button;
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
    private File2Service file2Service;

    @ViewComponent
    private CollectionContainer<BucketDto> bucketsDc;

    @ViewComponent
    private TreeDataGrid<BucketDto> bucketsTable;

    @ViewComponent
    private TypedTextField<String> prefixField;

    @ViewComponent
    private CollectionContainer<FileDto> filesDc;

    @ViewComponent
    private DataGrid<FileDto> filesTable;

    @Autowired
    private FileService fileService;

    @Subscribe
    public void onInit(InitEvent event){
        initBucketHierarchyColumn();
        loadAllBuckets();
        initFileTableColumns();
    }

    private void loadAllBuckets() {
        try {
            // Lấy danh sách bucket dạng tree (root level là các bucket)
            List<BucketDto> list = bucketService.listBucketFolderTree();

            // Gán danh sách bucket vào DataContainer
            bucketsDc.setItems(list);

            bucketsTable.addSelectionListener(e -> loadFiles());
        } catch (Exception e) {
            Notification.show("Load buckets failed: " + e.getMessage());
        }
    }

    private void loadAllFilesFromBucket() {
        try {
            String bucketName = getSelectedBucketName();
//            if (bucketName == null || bucketName.isBlank()) {
//                filesDc.setItems(List.of());
//                Notification.show("Chọn bucket để xem các file bên trong");
//                return;
//            }
            String prefix = prefixField.getValue();
            List<FileDto> list = fileService.getAllFromBucket(bucketName, prefix == null ? "" : prefix);
            filesDc.setItems(list);
            Notification.show("Danh sách folder and file của bucket: " + bucketName);

        } catch (Exception e) {
            Notification.show("Load folder and file failed: " + e.getMessage());
        }
    }

    private String getSelectedBucketName() {
        BucketDto selected = bucketsTable == null ? null : bucketsTable.getSingleSelectedItem();
        if (selected == null) return null;

        // đi ngược lên root (bucket gốc)
        BucketDto root = selected;
        while (root.getParent() != null) {
            root = root.getParent();
        }
        return root.getBucketName();
    }

    private void initBucketHierarchyColumn() {
        if (bucketsTable.getColumnByKey("name") != null) {
            bucketsTable.removeColumn(bucketsTable.getColumnByKey("name"));
        }

        TreeDataGrid.Column<BucketDto> nameColumn =
                bucketsTable.addComponentHierarchyColumn(this::renderFolderItem);

        nameColumn.setHeader("Bucket");
        bucketsTable.setColumnPosition(nameColumn, 0);
    }

    private HorizontalLayout renderFolderItem(BucketDto item) {
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

    @Subscribe(id = "deleteBucketBtn", subject = "clickListener")
    public void onDeleteBucketBtnClick(final ClickEvent<JmixButton> event) {
        try {
            Set<BucketDto> selectedItems = bucketsTable.getSelectedItems();
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

    private void loadFiles() {
        try {
            BucketDto selected = bucketsTable.getSingleSelectedItem();
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

            List<FileDto> list = fileService.getAllFromBucket(bucketName, prefix);
            filesDc.setItems(list);

        } catch (Exception e) {
            Notification.show("Load error: " + e.getMessage());
        }
    }

    private void initFileTableColumns() {
        // remove old name column if exists
        if (filesTable.getColumnByKey("name") != null) {
            filesTable.removeColumn(filesTable.getColumnByKey("name"));
        }

        // name column (component column)
        DataGrid.Column<FileDto> nameColumn =
                filesTable.addComponentColumn(this::renderFileItem).setKey("name");
        nameColumn.setHeader("File / Folder");
        filesTable.setColumnPosition(nameColumn, 0);

        // remove old type column if exists
        if (filesTable.getColumnByKey("type") != null) {
            filesTable.removeColumn(filesTable.getColumnByKey("type"));
        }
        DataGrid.Column<FileDto> typeColumn = filesTable
                .addColumn(item -> {
                    TreeNode t = item.getType();          // item.getType() trả TreeNode
                    return t == null ? "" : t.toString(); // toString() bạn đã override -> "File"/"Folder"
                })
                .setHeader("Type")
                .setKey("type");

        filesTable.setColumnPosition(typeColumn, 2);
    }


    private HorizontalLayout renderFileItem(FileDto item) {
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

    @Subscribe("filesTable")
    public void onFileDoubleClick(final ItemDoubleClickEvent<FileDto> event) {
        FileDto item = event.getItem();
        if (item == null) return;
        if (TreeNode.FOLDER.equals(item.getType())) {
            String bucketName = getSelectedBucketName();
            if (bucketName == null) {
                Notification.show("Không tìm thấy bucket");
                return;
            }
            try {
                String currentPrefix = prefixField.getValue();
                if (currentPrefix == null) currentPrefix = "";
                String newPrefix = (currentPrefix.isEmpty() ? "" : currentPrefix) + item.getName() + "/";
                // nếu chưa load children thì gọi fileService để lấy
                if (item.getChildren() == null || item.getChildren().isEmpty()) {
                    List<FileDto> children = file2Service.listLevel(bucketName, item.getKey() == null ? item.getName() + "/" : item.getKey());
                    item.setChildren(children);
                }
                filesDc.setItems(item.getChildren());
                prefixField.setValue(newPrefix);
                Notification.show("Đã load folder: " + item.getName());
            } catch (Exception e) {
                Notification.show("Load folder con thất bại: " + e.getMessage());
            }
        }
        else if (TreeNode.FILE.equals(item.getType())) {
            Notification.show("Double click file: " + item.getName());
        }
    }

    @Subscribe("backBtn")
    public void onBackBtnClick(ClickEvent<Button> event) {
        String bucketName = getSelectedBucketName();
        String currentKey = prefixField.getValue(); // key hiện tại (ví dụ: "folder1/folder2/")
        if (currentKey == null || currentKey.isEmpty()) {
            Notification.show("Đang ở thư mục gốc, không thể back");
            return;
        }
        // Xóa dấu "/" ở cuối
        String temp = currentKey.endsWith("/")
                ? currentKey.substring(0, currentKey.length() - 1)
                : currentKey;
        // Tìm slash cuối cùng
        int lastSlash = temp.lastIndexOf('/');

        String parentKey;
        if (lastSlash == -1) {
            // Không còn "/" => về root
            parentKey = "";
        } else {
            parentKey = temp.substring(0, lastSlash + 1); // Giữ "/" cuối cùng
        }
        try {
            // Load lại nội dung của thư mục cha
            List<FileDto> parentChildren = file2Service.listLevel(bucketName, parentKey);
            filesDc.setItems(parentChildren);

            // Cập nhật prefixField = parentKey
            prefixField.setValue(parentKey);

            Notification.show("Đã back về: " + (parentKey.isEmpty() ? "root" : parentKey));
        } catch (Exception e) {
            Notification.show("Load folder cha thất bại: " + e.getMessage());
        }
    }

}
