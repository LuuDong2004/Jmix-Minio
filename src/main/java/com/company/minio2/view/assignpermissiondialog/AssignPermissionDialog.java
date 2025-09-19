package com.company.minio2.view.assignpermissiondialog;

import com.company.minio2.entity.*;
import com.company.minio2.service.minio.SecurityService;
import com.company.minio2.view.advancesecurity.AdvanceSecurity;
import com.company.minio2.view.editpermission.EditPermission;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;

import com.vaadin.flow.component.checkbox.Checkbox;
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
import io.jmix.securitydata.entity.ResourceRoleEntity;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

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
    private CollectionLoader<ResourceRoleEntity> rolesDl;

    @ViewComponent
    private CollectionLoader<User> usersDl;

    @ViewComponent
    private DataGrid<Permission> permissionDataGrid;

    @ViewComponent
    private DataGrid<ObjectDTO> objectDTODataGrid;

    @ViewComponent
    private CollectionContainer<ObjectDTO> objectDtosDc;

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public void setUserName(List<User> username) {
        this.username = username;
    }

    @Subscribe(id = "editBtn", subject = "clickListener")
    public void onEditBtnClick(final ClickEvent<JmixButton> event) {
        ObjectDTO seleted = objectDTODataGrid.getSingleSelectedItem();
        DialogWindow<EditPermission> window = dialogWindows.view(this, EditPermission.class).build();
        window.getView().setFilePath(filePath);
        window.getView().setTarget(seleted);
        window.open();
    }

    @Subscribe
    public void onInit(InitEvent event) {

        // Cột Allow
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

        // Cột Deny
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

        // Lắng nghe khi chọn User/Role
        objectDTODataGrid.addSelectionListener(selection -> {
            Optional<ObjectDTO> optional = selection.getFirstSelectedItem();
            if (optional.isPresent()) {
                ObjectDTO dto = optional.get();

                CollectionContainer<Permission> permissionsDc =
                        getViewData().getContainer("permissionsDc");

                if (dto.getType() == ObjectType.USER) {
                    // Load User từ DB
                    User user = dataManager.load(User.class)
                            .id(UUID.fromString(dto.getId()))
                            .one();
                    selectedUser = user;

                    // Load quyền từ DB
                    Permission dbPermission = securityService.loadPermission(user, filePath);
                    int mask = dbPermission != null && dbPermission.getPermissionMask() != null
                            ? dbPermission.getPermissionMask()
                            : 0;

                    // Build danh sách quyền cho User
                    List<Permission> list = new ArrayList<>();
                    for (PermissionType type : PermissionType.values()) {
                        Permission p = dataManager.create(Permission.class);
                        p.setUser(user);
                        p.setFilePath(filePath);
                        p.setPermissionType(type);
                        p.setAllow(PermissionType.hasPermission(mask, type));
                        list.add(p);
                    }

                    permissionsDc.setItems(list);
                } else if (dto.getType() == ObjectType.ROLE) {
                    // Load Role từ DB
                    ResourceRoleEntity role = dataManager.load(ResourceRoleEntity.class)
                            .id(UUID.fromString(dto.getId()))   // id chính là code của role
                            .one();

                    // Load quyền của Role từ DB
                    Permission dbPermission = securityService.loadPermission(role, filePath);
                    int mask = dbPermission != null && dbPermission.getPermissionMask() != null
                            ? dbPermission.getPermissionMask()
                            : 0;

                    // Build danh sách quyền cho Role
                    List<Permission> list = new ArrayList<>();
                    for (PermissionType type : PermissionType.values()) {
                        Permission p = dataManager.create(Permission.class);
                        p.setRoleCode(role.getCode());
                        p.setFilePath(filePath);
                        p.setPermissionType(type);
                        p.setAllow(PermissionType.hasPermission(mask, type));
                        list.add(p);
                    }

                    permissionsDc.setItems(list);
                }
            }
        });
    }


    @Subscribe(id = "usersBtn", subject = "clickListener")
    public void onClickUsersButton(final ClickEvent<JmixButton> event) {
        usersDl.setParameter("filePath", filePath);
        usersDl.load();
        List<User> userList = usersDl.getContainer().getItems();
        List<ObjectDTO> dtos = userList.stream().map(
                u -> new ObjectDTO(u.getId().toString(), ObjectType.USER, u.getUsername())
        ).toList();
        objectDtosDc.setItems(dtos);
    }

    @Subscribe(id = "rolesBtn", subject = "clickListener")
    public void onClickRolesButton(final ClickEvent<JmixButton> event) {
        rolesDl.setParameter("filePath", filePath);
        rolesDl.load();
        List<ResourceRoleEntity> roles = rolesDl.getContainer().getItems();
        List<ObjectDTO> objectDTOs = roles.stream().map(
                r -> new ObjectDTO(r.getId().toString(), ObjectType.ROLE, r.getName())).toList();
        objectDtosDc.setItems(objectDTOs);
    }

    @Subscribe
    public void onBeforeShow(BeforeShowEvent event) {
        if (filePath != null) {
            fileKeyArea.setValue(filePath);
            usersDl.setParameter("filePath", filePath);
            usersDl.load();
        }
    }

    @Subscribe(id = "advanceBtn", subject = "clickListener")
    public void onAdvanceBtnClick(final ClickEvent<JmixButton> event) {
        ObjectDTO seleted = objectDTODataGrid.getSingleSelectedItem();
        DialogWindow<AdvanceSecurity> window = dialogWindows.view(this, AdvanceSecurity.class).build();
        window.getView().setFilePath(filePath);
        window.getView().setTarget(seleted);
        window.open();
    }

}