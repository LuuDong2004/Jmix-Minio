package com.company.minio2.view.confirmreplacedialog;


import com.company.minio2.entity.User;
import com.company.minio2.service.minio.SecurityService;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.router.Route;
import io.jmix.flowui.kit.component.button.JmixButton;
import io.jmix.flowui.view.*;
import io.jmix.securitydata.entity.ResourceRoleEntity;
import org.springframework.beans.factory.annotation.Autowired;


@Route(value = "confirm-replace-dialog", layout = MainView.class)
@ViewController(id = "ConfirmReplaceDialog")
@ViewDescriptor(path = "confirm-replace-dialog.xml")
public class ConfirmReplaceDialog extends StandardView {

    @ViewComponent
    private Span message;

    String path = "";

    String filePath = "";

    private User user;

    private ResourceRoleEntity role;

    private Integer permissionMask;

    @Autowired
    private SecurityService securityService;

    public void setPath(String folderName) {
        if (message != null) {
            message.setText(
                    "⚠ This will replace explicitly defined permissions on all descendants of this object with inheritable permissions from "
                            + folderName
            );
        }
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public void setRole(ResourceRoleEntity role) {
        this.role = role;
    }

    public void setPermissionMask(Integer permissionMask) {
        this.permissionMask = permissionMask;
    }

    @Subscribe(id = "yesBtn", subject = "clickListener")
    public void onYesBtnClick(final ClickEvent<JmixButton> event) {
        if (filePath != null && permissionMask != null) {
            if (user != null) {
                securityService.replaceChildPermissions(user, filePath, permissionMask);
            } else if (role != null) {
                securityService.replaceChildPermissions(role, filePath, permissionMask);
            }
        }
        close(StandardOutcome.SAVE); // đóng dialog, báo cho view cha biết
    }

    @Subscribe(id = "noBtn", subject = "clickListener")
    public void onNoBtnClick(final ClickEvent<JmixButton> event) {
        close(StandardOutcome.CLOSE);
    }


}