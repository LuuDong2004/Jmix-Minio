package com.company.minio2.view.assignpermissiondialog;


import com.company.minio2.entity.Permission;
import com.company.minio2.entity.User;
import com.company.minio2.view.editpermission.EditPermission;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.router.Route;
import io.jmix.flowui.DialogWindows;
import io.jmix.flowui.kit.component.button.JmixButton;
import io.jmix.flowui.model.CollectionLoader;
import io.jmix.flowui.view.*;

import com.vaadin.flow.component.textfield.TextArea;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

@Route(value = "assign-permission-dialog", layout = MainView.class)
@ViewController(id = "AssignPermissionDialog")
@ViewDescriptor(path = "assign-permission-dialog.xml")
public class AssignPermissionDialog extends StandardView {

    @Autowired
    private DialogWindows dialogWindows;

    @Subscribe(id = "editBtn", subject = "clickListener")
    public void onEditBtnClick(final ClickEvent<JmixButton> event) {
        DialogWindow<EditPermission> window = dialogWindows.view(this, EditPermission.class).build();
        window.open();
    }

    String filePath = "";

    List<User> username = new ArrayList<>();

    @ViewComponent
    private TextArea fileKeyArea;

    @ViewComponent
    private CollectionLoader<Permission> permissionsDl;

    @ViewComponent
    private CollectionLoader<User> usersDl;

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public void setUserName(List<User> username) {
        this.username = username;
    }


    @Subscribe
    public void onBeforeShow(BeforeShowEvent event) {
        if (filePath != null) {
            fileKeyArea.setValue(filePath);
            permissionsDl.setParameter("filePath", filePath);
            usersDl.load();
            permissionsDl.load();

            System.out.println("DEBUG >>> sau khi load, size = " + permissionsDl.getContainer().getItems().size());
        }
    }
}