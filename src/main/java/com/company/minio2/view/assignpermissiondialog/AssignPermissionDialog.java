package com.company.minio2.view.assignpermissiondialog;

import com.company.minio2.dto.ObjectDto;
import com.company.minio2.entity.Permission;
import com.company.minio2.entity.PermissionType;
import com.company.minio2.entity.User;
import com.company.minio2.service.minio.SecurityService;
import com.company.minio2.view.editpermission.EditPermission;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;

import com.vaadin.flow.component.checkbox.Checkbox;

import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.Route;
import io.jmix.core.DataManager;
import io.jmix.flowui.DialogWindows;
import io.jmix.flowui.component.grid.DataGrid;
import io.jmix.flowui.kit.component.button.JmixButton;
import io.jmix.flowui.model.CollectionContainer;
import io.jmix.flowui.model.CollectionLoader;
import io.jmix.flowui.view.*;

import com.vaadin.flow.component.textfield.TextArea;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Route(value = "assign-permission-dialog", layout = MainView.class)
@ViewController(id = "AssignPermissionDialog")
@ViewDescriptor(path = "assign-permission-dialog.xml")
public class AssignPermissionDialog extends StandardView {

    @Autowired
    private SecurityService securityService;

    @Autowired
    private DialogWindows dialogWindows;

    @Autowired
    private DataManager dataManager;

    private User selectedUser;

    String filePath = "";

    List<User> username = new ArrayList<>();

    @ViewComponent
    private TextArea fileKeyArea;

    @ViewComponent
    private CollectionLoader<Permission> permissionsDl;

    @ViewComponent
    private CollectionLoader<User> usersDl;

    @ViewComponent
    private DataGrid<Permission> permissionDataGrid;

    @ViewComponent
    private DataGrid<User> usersDataGrid;

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public void setUserName(List<User> username) {
        this.username = username;
    }

    @Subscribe(id = "editBtn", subject = "clickListener")
    public void onEditBtnClick(final ClickEvent<JmixButton> event) {
        User seleted = usersDataGrid.getSingleSelectedItem();
        DialogWindow<EditPermission> window = dialogWindows.view(this, EditPermission.class).build();
        window.getView().setFilePath(filePath);
        window.getView().setUserName(List.of(seleted));
        window.open();
    }

    @Subscribe
    public void onInit(InitEvent event) {
        permissionDataGrid.addColumn(
                new ComponentRenderer<>(permission -> {
                    Checkbox checkbox = new Checkbox();
                    checkbox.setValue(Boolean.TRUE.equals(permission.getAllow()));
                    checkbox.setReadOnly(true);
                    checkbox.addValueChangeListener(e -> {
                        if (e.getValue()) {
                            permission.setAllow(true);
                        } else if (Boolean.TRUE.equals(permission.getAllow())) {
                            permission.setAllow(null);
                        }
                        permissionDataGrid.getDataProvider().refreshItem(permission);
                    });
                    return checkbox;
                })
        ).setHeader("Allow");

        permissionDataGrid.addColumn(
                new ComponentRenderer<>(permission -> {
                    Checkbox checkbox = new Checkbox();
                    checkbox.setValue(Boolean.FALSE.equals(permission.getAllow()));
                    checkbox.setReadOnly(true);
                    checkbox.addValueChangeListener(e -> {
                        if (e.getValue()) {
                            permission.setAllow(false);
                        }
                    });
                    return checkbox;
                })
        ).setHeader("Deny");

        usersDataGrid.addSelectionListener(selection -> {
            Optional<User> user = selection.getFirstSelectedItem();
            if (user.isPresent()) {
                selectedUser = user.get();

                // Load từ DB xem user này có permission gì
                Permission dbPermission = securityService.loadPermission(selectedUser, filePath);
                int mask = dbPermission != null && dbPermission.getPermissionMask() != null
                        ? dbPermission.getPermissionMask()
                        : 0;

                // Tạo list quyền từ enum
                List<Permission> list = new ArrayList<>();
                for (PermissionType type : PermissionType.values()) {
                    Permission p = dataManager.create(Permission.class);
                    p.setUser(selectedUser);
                    p.setFilePath(filePath);
                    p.setPermissionType(type);
                    p.setAllow(PermissionType.hasPermission(mask, type));
                    list.add(p);
                }

                CollectionContainer<Permission> permissionsDc =
                        getViewData().getContainer("permissionsDc");
                permissionsDc.setItems(list);
            }
        });

    }

    @Subscribe
    public void onBeforeShow(BeforeShowEvent event) {
        if (filePath != null) {
            fileKeyArea.setValue(filePath);
            permissionsDl.setParameter("filePath", filePath);
            permissionsDl.setParameter("user", selectedUser);
            usersDl.load();
        }
    }

}