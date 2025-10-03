package com.company.minio2.view.blockinheritance;


import com.company.minio2.entity.User;
import com.company.minio2.service.minio.SecurityService;
import com.company.minio2.view.main.MainView;
import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.router.Route;
import io.jmix.flowui.kit.component.button.JmixButton;
import io.jmix.flowui.view.*;
import io.jmix.securitydata.entity.ResourceRoleEntity;
import org.springframework.beans.factory.annotation.Autowired;

@Route(value = "block-inheritance", layout = MainView.class)
@ViewController(id = "BlockInheritance")
@ViewDescriptor(path = "block-inheritance.xml")
public class BlockInheritance extends StandardView {

    @Autowired
    private SecurityService securityService;

    private User targetUser;
    private String filePath;
    private ResourceRoleEntity resourceRole;

    public void setTargetUser(User user, String filePath) {
        this.targetUser = user;
        this.filePath = filePath;
    }

    public void setTargetRole(ResourceRoleEntity resourceRole, String filePath) {
        this.resourceRole = resourceRole;
        this.filePath = filePath;
    }

    @Subscribe(id = "convertBtn", subject = "clickListener")
    public void onConvertBtnClick(final ClickEvent<JmixButton> event) {
        if (filePath != null) {
            if (targetUser != null) {
                securityService.disableInheritance(targetUser, filePath, true);
            } else if (resourceRole != null) {
                securityService.disableInheritance(resourceRole, filePath, true);
            }
            close(StandardOutcome.SAVE);
        }
    }

    @Subscribe(id = "removeBtn", subject = "clickListener")
    public void onRemoveBtnClick(final ClickEvent<JmixButton> event) {
        if (filePath != null) {
            if (targetUser != null) {
                securityService.disableInheritance(targetUser, filePath, false);
            } else if (resourceRole != null) {
                securityService.disableInheritance(resourceRole, filePath, false);
            }
            close(StandardOutcome.SAVE);
        }
    }

    @Subscribe(id = "cancelBtn", subject = "clickListener")
    public void onCancelBtnClick(final ClickEvent<JmixButton> event) {
        close(StandardOutcome.CLOSE);
    }

}