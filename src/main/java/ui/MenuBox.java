package ui;

import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.VBox;
import javafx.stage.FileChooser;
import lombok.Getter;

@Getter
public final class MenuBox
{
    private final VBox box;

    public MenuBox()
    {
        this.box = generateMenuBox();
    }

    private static VBox generateMenuBox()
    {
        return new VBox(generateMenuBar());
    }

    private static MenuBar generateMenuBar()
    {
        MenuBar menuBar = new MenuBar();
        menuBar.getMenus().addAll(createOpenMenu(), createEditMenu(), createFormatMenu(),
                                  createRunMenu(), createOptionsMenu(), createHelpMenu());

        return menuBar;
    }

    private static Menu createOpenMenu()
    {
        Menu openMenu = new Menu("Open");

        MenuItem open = new MenuItem("File");
        MenuItem save = new MenuItem("Save");
        MenuItem saveAs = new MenuItem("Save As");
        openMenu.getItems().addAll(open, save, saveAs);

        FileChooser fileChooser = new FileChooser();
        open.setOnAction(e -> {
            //File selectedFile = fileChooser.showOpenDialog(stage);
        });

        return openMenu;
    }

    private static Menu createEditMenu()
    {
        Menu editMenu = new Menu("Edit");
        return editMenu;
    }

    private static Menu createFormatMenu()
    {
        Menu formatMenu = new Menu("Format");
        return formatMenu;
    }

    private static Menu createRunMenu()
    {
        Menu runMenu = new Menu("Run");
        return runMenu;
    }

    private static Menu createOptionsMenu()
    {
        Menu optionsMenu = new Menu("Options");
        return optionsMenu;
    }

    private static Menu createHelpMenu()
    {
        Menu helpMenu = new Menu("Help");
        return helpMenu;
    }
}
