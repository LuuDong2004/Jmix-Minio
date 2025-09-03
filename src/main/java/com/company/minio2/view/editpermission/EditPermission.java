package com.company.minio2.view.editpermission;


import com.company.minio2.view.main.MainView;
import com.vaadin.flow.router.Route;
import io.jmix.flowui.view.StandardView;
import io.jmix.flowui.view.ViewController;
import io.jmix.flowui.view.ViewDescriptor;

@Route(value = "edit-permission", layout = MainView.class)
@ViewController(id = "EditPermission")
@ViewDescriptor(path = "edit-permission.xml")
public class EditPermission extends StandardView {

}