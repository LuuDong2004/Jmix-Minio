package com.company.minio2.view.editpermission;


import com.company.minio2.entity.*;
import com.company.minio2.entity.Object;
import com.company.minio2.service.minio.SecurityService;
import com.company.minio2.view.main.MainView;
import com.company.minio2.view.userlistdialog.UserListDialog;
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
import io.jmix.securitydata.entity.ResourceRoleEntity;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Route(value = "edit-permission", layout = MainView.class)
@ViewController(id = "EditPermission")
@ViewDescriptor(path = "edit-permission.xml")
public class EditPermission extends StandardView {

    @Autowired
    private SecurityService securityService;

    @Autowired
    private DataManager dataManager;
    @Autowired
    private DialogWindows dialogWindows;

    private ResourceRoleEntity selectedRole;

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
    private DataGrid<Object> objectDTODataGrid;

    @ViewComponent
    private CollectionContainer<Object> objectDtosDc;

    @ViewComponent
    private DataGrid<User> usersDataGrid;

    @ViewComponent
    private CollectionLoader<ResourceRoleEntity> rolesDl;

    private Object target;

    @ViewComponent
    private CollectionContainer<Permission> permissionsDc;

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public void setUserName(List<User> username) {
        this.username = username;
    }

    public void setTarget(Object target) {
        this.target = target;
    }

    @Subscribe
    public void onInit(InitEvent event) {

        // Cột Allow
        permissionDataGrid.addColumn(
                new ComponentRenderer<>(permission -> {
                    Checkbox checkbox = new Checkbox();
                    checkbox.setValue(Boolean.TRUE.equals(permission.getAllow()));
                    checkbox.addValueChangeListener(e -> {
                        if (e.getValue()) {
                            permission.setAllow(true);
                            if (permission.getPermissionType().equals(PermissionType.MODIFY)) {
                                CollectionContainer<Permission> permissionsDc = getViewData().getContainer("permissionsDc");
                                for (Permission p : permissionsDc.getItems()) {
                                    if (p.getPermissionType().equals(PermissionType.READ)
                                            || p.getPermissionType().equals(PermissionType.CREATE)) {
                                        p.setAllow(true);
                                        permissionDataGrid.getDataProvider().refreshItem(p);
                                    }
                                }
                            }
                        } else {
                            permission.setAllow(false);
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
                    checkbox.addValueChangeListener(e -> {
                        if (e.getValue()) {
                            permission.setAllow(false);
                            if (permission.getPermissionType().equals(PermissionType.READ)) {
                                CollectionContainer<Permission> permissionsDc = getViewData().getContainer("permissionsDc");
                                for (Permission p : permissionsDc.getItems()) {
                                    if (p.getPermissionType().equals(PermissionType.CREATE)
                                    || p.getPermissionType().equals(PermissionType.MODIFY)) {
                                        p.setAllow(false);
                                        permissionDataGrid.getDataProvider().refreshItem(p);
                                    }
                                }
                            }
                        } else {
                            permission.setAllow(true);
                        }
                        permissionDataGrid.getDataProvider().refreshItem(permission);
                    });
                    return checkbox;
                })
        ).setHeader("Deny");

        // Lắng nghe khi chọn User/Role
        objectDTODataGrid.addSelectionListener(selection -> {
            Optional<Object> optional = selection.getFirstSelectedItem();
            if (optional.isPresent()) {
                Object dto = optional.get();

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
                            .query("select r from sec_ResourceRoleEntity r where r.code = :code")
                            .parameter("code", dto.getId())
                            .one();
                    selectedRole = role;
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

    @Subscribe
    public void onBeforeShow(BeforeShowEvent event) {
        if (filePath != null) {
            fileKeyArea.setValue(filePath);
            usersDl.setParameter("filePath", filePath);
            usersDl.load();
        }
    }

    @Subscribe("usersBtn")
    public void onUsersBtnClick(ClickEvent<JmixButton> event) {
        usersDl.setParameter("filePath", filePath);
        usersDl.load();
        List<User> users = usersDl.getContainer().getItems();
        List<Object> dtos = new ArrayList<>();
        for (User u : users) {
            Object dto = new Object();
            dto.setId(u.getId().toString());   // để sau này load User
            dto.setName(u.getUsername());
            dto.setType(ObjectType.USER);
            dtos.add(dto);
        }
        objectDtosDc.setItems(dtos);
    }

    @Subscribe("rolesBtn")
    public void onRolesBtnClick(ClickEvent<JmixButton> event) {
        rolesDl.setParameter("filePath", filePath);
        rolesDl.load();
        List<ResourceRoleEntity> roles = rolesDl.getContainer().getItems();
        List<Object> dtos = new ArrayList<>();
        for (ResourceRoleEntity r : roles) {
            Object dto = new Object();
            dto.setId(r.getCode());    // code dùng làm key cho role
            dto.setName(r.getName());
            dto.setType(ObjectType.ROLE);
            dtos.add(dto);
        }
        objectDtosDc.setItems(dtos);
    }

    @Subscribe(id = "saveBtn", subject = "clickListener")
    public void onSaveBtnClick(final ClickEvent<JmixButton> event) {
        CollectionContainer<Permission> permissionDc = getViewData().getContainer("permissionsDc");
        if (selectedUser != null) {
            securityService.savePermission(permissionDc.getItems(), selectedUser, filePath);
        } else if (selectedRole != null) {
            securityService.savePermission(permissionDc.getItems(), selectedRole, filePath);
        }
        Notification.show("Permission saved");
        close(StandardOutcome.SAVE);
    }

    @Subscribe(id = "addBtn", subject = "clickListener")
    public void onAddBtnClick(final ClickEvent<JmixButton> event) {
        DialogWindow<UserListDialog> window = dialogWindows.view(this, UserListDialog.class).build();
        window.getView().setFilePath(filePath);
        window.addAfterCloseListener(afterCloseEvent -> {
            if (afterCloseEvent.closedWith(StandardOutcome.SAVE)) {
                usersDl.setParameter("filePath", filePath);
                usersDl.load();
                List<Object> dtos = new ArrayList<>();
                for (User u : usersDl.getContainer().getItems()) {
                    Object dto = new Object();
                    dto.setId(u.getId().toString());
                    dto.setName(u.getUsername());
                    dto.setType(ObjectType.USER);
                    dtos.add(dto);
                }

                // reload roles
                rolesDl.setParameter("filePath", filePath);
                rolesDl.load();
                for (ResourceRoleEntity r : rolesDl.getContainer().getItems()) {
                    Object dto = new Object();
                    dto.setId(r.getId().toString());
                    dto.setName(r.getName());
                    dto.setType(ObjectType.ROLE);
                    dtos.add(dto);
                }

                // update UI
                objectDtosDc.setItems(dtos);
            }
        });
        window.open();
    }

}