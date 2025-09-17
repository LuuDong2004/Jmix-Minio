package com.company.minio2.view.userlistdialog;


import com.company.minio2.entity.ObjectDTO;
import com.company.minio2.entity.ObjectType;
import com.company.minio2.entity.Permission;
import com.company.minio2.entity.User;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.Route;
import io.jmix.core.DataManager;
import io.jmix.flowui.component.grid.DataGrid;
import io.jmix.flowui.kit.component.button.JmixButton;
import io.jmix.flowui.model.CollectionContainer;
import io.jmix.flowui.model.CollectionLoader;
import io.jmix.flowui.view.*;
import io.jmix.securitydata.entity.ResourceRoleEntity;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Route(value = "user-list-dialog", layout = MainView.class)
@ViewController(id = "UserListDialog")
@ViewDescriptor(path = "user-list-dialog.xml")
public class UserListDialog extends StandardView {

    String filePath = "";

    @ViewComponent
    private TextArea fileKeyArea;

    @ViewComponent
    private CollectionLoader<Permission> permissionsDl;

    @ViewComponent
    private CollectionLoader<User> usersDl;

    private User selectedUser;

    @ViewComponent
    private DataGrid<ObjectDTO> objectDTODataGrid;

    @ViewComponent
    private CollectionContainer<ObjectDTO> objectDtosDc;

    @Autowired
    private DataManager dataManager;

    @ViewComponent
    private CollectionLoader<ResourceRoleEntity> rolesDl;

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    @Subscribe
    public void onBeforeShow(BeforeShowEvent event) {
        if (filePath != null) {
            fileKeyArea.setValue(filePath);
            permissionsDl.setParameter("filePath", filePath);
            permissionsDl.setParameter("user", selectedUser);
            usersDl.setParameter("filePath", filePath);
            usersDl.load();
        }
    }

    @Subscribe
    private void onInit(InitEvent event) {
        objectDTODataGrid.addColumn(
                new ComponentRenderer<>(permission -> {
                    Checkbox checkbox = new Checkbox();
                    checkbox.setValue(Boolean.TRUE.equals(permission.getSelected()));
                    checkbox.addValueChangeListener(e -> {
                        permission.setSelected(e.getValue());
                    });
                    return checkbox;
                })
        ).setHeader("Add");
    }

    @Subscribe("usersBtn")
    public void onUsersBtnClick(ClickEvent<JmixButton> event) {
        usersDl.setParameter("filePath", filePath);
        usersDl.load();
        List<User> users = usersDl.getContainer().getItems();
        List<ObjectDTO> dtos = new ArrayList<>();
        for (User u : users) {
            ObjectDTO dto = new ObjectDTO();
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
        List<ObjectDTO> dtos = new ArrayList<>();
        for (ResourceRoleEntity r : roles) {
            ObjectDTO dto = new ObjectDTO();
            dto.setId(r.getCode());    // code dùng làm key cho role
            dto.setName(r.getName());
            dto.setType(ObjectType.ROLE);
            dtos.add(dto);
        }
        objectDtosDc.setItems(dtos);
    }

    @Subscribe(id = "applyBtn", subject = "clickListener")
    public void onApplyBtnClick(final ClickEvent<JmixButton> event) {
        List<ObjectDTO> selectedObjects = new ArrayList<>();
        for (ObjectDTO dto : objectDtosDc.getItems()) {
            if (Boolean.TRUE.equals(dto.getSelected())) {
                selectedObjects.add(dto);
            }
        }
        List<Permission> toSave = new ArrayList<>();
        // 2 biến để sau reload
        User selectedUser = null;
        String selectedRoleCode = null;
        for (ObjectDTO dto : selectedObjects) {
            Permission p = dataManager.create(Permission.class);
            p.setFilePath(filePath);
            if (dto.getType() == ObjectType.USER) {
                User user = dataManager.load(User.class).id(UUID.fromString(dto.getId())).one();
                p.setUser(user);
                selectedUser = user;   // lưu lại để reload
            } else if (dto.getType() == ObjectType.ROLE) {
                p.setRoleCode(dto.getId()); // id = roleCode
                selectedRoleCode = dto.getId();  // lưu lại để reload
            }
            // p.setPermissionType(PermissionType.ALLOW);
            toSave.add(p);
        }
        dataManager.save(toSave.toArray());
        // reload lại permission list
        permissionsDl.setParameter("filePath", filePath);
        permissionsDl.setParameter("user", selectedUser);
        permissionsDl.setParameter("roleCode", selectedRoleCode);
        permissionsDl.load();

        close(StandardOutcome.SAVE);
    }
}