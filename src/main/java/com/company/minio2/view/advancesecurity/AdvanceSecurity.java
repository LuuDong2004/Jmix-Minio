package com.company.minio2.view.advancesecurity;


import com.company.minio2.entity.*;
import com.company.minio2.service.minio.SecurityService;
import com.company.minio2.view.blockinheritance.BlockInheritance;
import com.company.minio2.view.confirmreplacedialog.ConfirmReplaceDialog;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.router.Route;
import io.jmix.flowui.DialogWindows;
import io.jmix.flowui.component.checkbox.JmixCheckbox;
import io.jmix.flowui.component.grid.DataGrid;
import io.jmix.flowui.data.ContainerDataUnit;
import io.jmix.flowui.kit.component.button.JmixButton;
import io.jmix.flowui.model.CollectionContainer;
import io.jmix.flowui.model.CollectionLoader;
import io.jmix.flowui.view.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collections;

@Route(value = "advance-security", layout = MainView.class)
@ViewController(id = "AdvanceSecurity")
@ViewDescriptor(path = "advance-security.xml")
public class AdvanceSecurity extends StandardView {

    @Autowired
    private SecurityService securityService;

    @Autowired
    private DialogWindows dialogWindows;

    @ViewComponent
    private DataGrid<Permission> permissionDataGrid;

    @ViewComponent
    private JmixCheckbox replaceChildPermissions;

    @ViewComponent
    private JmixButton disableInheritanceBtn;

    @ViewComponent
    private TextArea fileKeyArea;

    String filePath = "";

    private ObjectDTO target;
    @ViewComponent
    private CollectionLoader<Permission> permissionsDl;

    public void setTarget(ObjectDTO target) {
        this.target = target;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    @Subscribe
    public void onBeforeShow(BeforeShowEvent event) {
        if (filePath != null) {
            fileKeyArea.setValue(filePath);
            getViewData().getLoader("permissionsDl").setParameter("filePath", filePath);
            getViewData().getLoader("permissionsDl").load();
        }

        // chọn permission đầu tiên để set nút
        Permission p = permissionDataGrid.getSingleSelectedItem();
        updateInheritanceButtonLabel(p);
    }

    private void updateInheritanceButtonLabel(Permission permission) {
        if (permission == null && permissionDataGrid.getItems() != null) {
            if (permissionDataGrid.getItems() instanceof ContainerDataUnit) {
                ContainerDataUnit<?> dataUnit = (ContainerDataUnit<?>) permissionDataGrid.getItems();
                if (dataUnit.getContainer() instanceof CollectionContainer) {
                    CollectionContainer<Permission> container = (CollectionContainer<Permission>) dataUnit.getContainer();
                    if (!container.getItems().isEmpty()) {
                        permission = container.getItems().get(0); // lấy phần tử đầu tiên
                    }
                }
            }
        }
        if (permission == null) {
            // Không có permission => coi như inheritance đã bị remove
            disableInheritanceBtn.setText("Enable Inheritance");
            return;
        }
        if (Boolean.FALSE.equals(permission.getInheritEnabled())) {
            disableInheritanceBtn.setText("Enable Inheritance");
        } else {
            disableInheritanceBtn.setText("Disable Inheritance");
        }
    }

    @Subscribe
    public void onInit(InitEvent event) {

        permissionDataGrid.addColumn(
                permission -> "Allow"
        ).setHeader("Type");

        permissionDataGrid.addColumn(permission -> {
            if (permission.getPermissionMask() == null) return "";

            int mask = permission.getPermissionMask();

            if (PermissionType.hasPermission(mask, PermissionType.FULL)) {
                return PermissionType.FULL.toString();
            }

            StringBuilder sb = new StringBuilder();

            for (PermissionType type : PermissionType.values()) {
                if (PermissionType.hasPermission(mask, type)) {
                    if (sb.length() > 0) {
                        sb.append(", ");
                    }
                    sb.append(type.toString());
                }
            }

            return sb.toString();
        }).setHeader("Access");

        permissionDataGrid.addColumn(
                permission -> {
                    if (permission.getInheritedFrom() == null) return "";
                    return permission.getInheritedFrom();
                }
        ).setHeader("Inherited from");

        permissionDataGrid.addColumn(
                permission -> {
                    AppliesTo applies = permission.getAppliesTo();
                    if (applies == null) return "";
                    switch (applies) {
                        case THIS_FOLDER_ONLY: return "This folder only";
                        case THIS_FOLDER_SUBFOLDERS_FILES: return "This folder, subfolders and files";
                        case THIS_FOLDER_SUBFOLDERS: return "This folder and subfolders";
                        case THIS_FOLDER_FILES: return "This folder and files";
                        case SUBFOLDERS_FILES_ONLY: return "Subfolders and files only";
                        default: return "";
                    }
                }
        ).setHeader("Applies to");
    }

    @Subscribe(id = "disableInheritanceBtn", subject = "clickListener")
    public void onDisableInheritanceBtnClick(final ClickEvent<JmixButton> event) {
        Permission permission = permissionDataGrid.getSingleSelectedItem();
        if (permission == null) {
            // Không có record nào (Remove case). Hãy enable inheritance cho tất cả principals tại filePath.
            if (filePath != null && !filePath.isBlank()) {
                securityService.enableRemoveInheritance(filePath);
                permissionsDl.load();
                updateInheritanceButtonLabel(permissionDataGrid.getSingleSelectedItem());
            }
            return;
        }

        if (Boolean.TRUE.equals(permission.getInheritEnabled())) {
            // Đang kế thừa → disable → mở dialog Convert/Remove
            DialogWindow<BlockInheritance> window = dialogWindows.view(this, BlockInheritance.class).build();
            window.getView().setTarget(permission.getUser(), permission.getFilePath());
            window.addAfterCloseListener(e -> {
                permissionsDl.load();
                updateInheritanceButtonLabel(permissionDataGrid.getSingleSelectedItem());
            });
            window.open();
        } else {
            // Đã disable → enable lại (user-specific)
            securityService.enableInheritance(permission.getUser(), permission.getFilePath());
            permissionsDl.load();
            updateInheritanceButtonLabel(permissionDataGrid.getSingleSelectedItem());
        }
    }


    @Subscribe(id = "applyBtn", subject = "clickListener")
    public void onApplyBtnClick(final ClickEvent<JmixButton> event) {
//        Permission permission = permissionDataGrid.getSingleSelectedItem();
//        if (permission == null) return;
//        boolean replace = Boolean.TRUE.equals(replaceChildPermissions.getValue());
//        if (replace) {
//            securityService.replaceChildPermissions(
//                    permission.getUser(),permission.getFilePath(), permission.getPermissionMask());
//        } else {
//            securityService.savePermission(
//                    Collections.singleton(permission),
//                    permission.getUser(),
//                    permission.getFilePath()
//            );
//        }
//        permissionsDl.load();
//        close(StandardOutcome.SAVE);
        Permission permission = permissionDataGrid.getSingleSelectedItem();
        if (permission == null) {
            return;
        }
        String folderName = filePath;
        if (folderName != null && !folderName.isBlank()) {
            // Bỏ dấu '/' cuối nếu có
            if (folderName.endsWith("/")) {
                folderName = folderName.substring(0, folderName.length() - 1);
            }
            // Lấy phần sau cùng
            int lastSlash = folderName.lastIndexOf('/');
            if (lastSlash >= 0 && lastSlash < folderName.length() - 1) {
                folderName = folderName.substring(lastSlash + 1);
            }
        }

        DialogWindow<ConfirmReplaceDialog> window = dialogWindows.view(this, ConfirmReplaceDialog.class).build();
        window.getView().setPath(folderName);
        window.getView().setFilePath(permission.getFilePath());
        window.getView().setUser(permission.getUser());
        window.getView().setPermissionMask(permission.getPermissionMask());

        // sau khi đóng dialog -> reload lại danh sách
        window.addAfterCloseListener(e -> {
            if (e.closedWith(StandardOutcome.SAVE)) {
                permissionsDl.load();
            }
        });

        window.open();
    }

    @Subscribe(id = "cancelBtn", subject = "clickListener")
    public void onCancelBtnClick(final ClickEvent<JmixButton> event) {
        close(StandardOutcome.CLOSE);
    }

}