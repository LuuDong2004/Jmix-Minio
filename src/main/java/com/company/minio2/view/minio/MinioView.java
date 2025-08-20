package com.company.minio2.view.minio;
import com.company.minio2.dto.BucketDto;
import com.company.minio2.dto.TreeNode;
import com.company.minio2.service.minio.IBucketService;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.router.Route;
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

    @ViewComponent
    private CollectionContainer<BucketDto> bucketsDc;

    @ViewComponent
    private TreeDataGrid<BucketDto> bucketsTable;

    @ViewComponent
    private TypedTextField<String> prefixField;

    @Subscribe
    public void onInit(InitEvent event){
        loadAllBuckets();
    }

    private void loadAllBuckets() {
        try {
            List<BucketDto> list = bucketService.listBucketFolderTree();
            bucketsDc.setItems(list);
        } catch (Exception e) {
            Notification.show("Load buckets failed: " + e.getMessage());
        }
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

}