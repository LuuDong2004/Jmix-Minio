package com.company.minio2.view.advancesecurity;


import com.company.minio2.entity.*;
import com.company.minio2.service.minio.SecurityService;
import com.company.minio2.view.blockinheritance.BlockInheritance;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.router.Route;
import io.jmix.flowui.DialogWindows;
import io.jmix.flowui.component.checkbox.JmixCheckbox;
import io.jmix.flowui.component.grid.DataGrid;
import io.jmix.flowui.kit.component.button.JmixButton;
import io.jmix.flowui.model.CollectionLoader;
import io.jmix.flowui.view.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.awt.*;
import java.util.ArrayList;
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
        if (permission == null) {
            disableInheritanceBtn.setText("Disable Inheritance");
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
        if (permission == null) return;

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
            // Đã disable → enable lại
            securityService.enableInheritance(permission.getUser(), permission.getFilePath());
            permissionsDl.load();
            updateInheritanceButtonLabel(permissionDataGrid.getSingleSelectedItem());
        }
    }

    @Subscribe(id = "applyBtn", subject = "clickListener")
    public void onApplyBtnClick(final ClickEvent<JmixButton> event) {
        Permission permission = permissionDataGrid.getSingleSelectedItem();
        if (permission == null) return;
        boolean replace = Boolean.TRUE.equals(replaceChildPermissions.getValue());
        if (replace) {
            securityService.replaceChildPermissions(
                    permission.getUser(),permission.getFilePath(), permission.getPermissionMask());
        } else {
            securityService.savePermission(
                    Collections.singleton(permission),
                    permission.getUser(),
                    permission.getFilePath()
            );
        }
        permissionsDl.load();
        close(StandardOutcome.SAVE);
    }
}