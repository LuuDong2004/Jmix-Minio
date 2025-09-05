package com.company.minio2.view.editpermission;


import com.company.minio2.entity.Permission;
import com.company.minio2.entity.PermissionType;
import com.company.minio2.entity.User;
import com.company.minio2.service.minio.SecurityService;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.Route;
import io.jmix.core.DataManager;
import io.jmix.flowui.DialogWindows;
import io.jmix.flowui.component.grid.DataGrid;
import io.jmix.flowui.kit.component.button.JmixButton;
import io.jmix.flowui.model.CollectionContainer;
import io.jmix.flowui.model.CollectionLoader;
import io.jmix.flowui.view.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Route(value = "edit-permission", layout = MainView.class)
@ViewController(id = "EditPermission")
@ViewDescriptor(path = "edit-permission.xml")
public class EditPermission extends StandardView {

    @Autowired
    private SecurityService securityService;

    @Autowired
    private DataManager dataManager;

    @Subscribe(id = "saveBtn", subject = "clickListener")
    public void onSaveBtnClick(final ClickEvent<JmixButton> event) {
        CollectionContainer<Permission> permissionDc = getViewData().getContainer("permissionsDc");
        securityService.savePermission(permissionDc.getItems(), selectedUser, filePath);
        Notification.show("Permission saved");

    }

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

    @Subscribe
    public void onInit(InitEvent event) {
        permissionDataGrid.addColumn(
                new ComponentRenderer<>(permission -> {
                    Checkbox checkbox = new Checkbox();
                    checkbox.setValue(Boolean.TRUE.equals(permission.getAllow()));
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
                permissionsDl.setParameter("filePath", filePath);
                permissionsDl.setParameter("user", selectedUser);
                permissionsDl.load();

                CollectionContainer<Permission> permissionsDc = getViewData().getContainer("permissionsDc");
                List<Permission> list = new ArrayList<>(permissionsDc.getItems());

                for (PermissionType type : PermissionType.values()) {
                    boolean exists = list.stream().anyMatch(p -> p.getPermissionType() == type);
                    if (!exists) {
                        Permission p = dataManager.create(Permission.class);
                        p.setUser(selectedUser);
                        p.setFilePath(filePath);
                        p.setPermissionType(type);
                        p.setAllow(null);
                        list.add(p);
                    }
                }
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