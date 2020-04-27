package ui;

import compilation.Compiler;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.VBox;
import lombok.Getter;
import source.SourceInput;
import source.SourceOutput;

@Getter
public final class MenuBox
{
    private final InputBox inputBox;
    private final OutputBox outputBox;
    private final VBox box;

    public MenuBox(InputBox inputBox, OutputBox outputBox)
    {
        this.inputBox = inputBox;
        this.outputBox = outputBox;
        this.box = generateMenuBox();
    }

    private VBox generateMenuBox()
    {
        return new VBox(generateMenuBar());
    }

    private MenuBar generateMenuBar()
    {
        MenuBar menuBar = new MenuBar();
        menuBar.getMenus().addAll(this.createFileMenu(), this.createEditMenu(),
                                  this.createFormatMenu(), this.createRunMenu(),
                                  this.createOptionsMenu(), this.createHelpMenu());

        return menuBar;
    }

    private Menu createFileMenu()
    {
        Menu openMenu = new Menu("File");
        MenuItem open = new MenuItem("Open");
        MenuItem save = new MenuItem("Save");
        MenuItem saveAs = new MenuItem("Save As");
        openMenu.getItems().addAll(open, save, saveAs);

        return openMenu;
    }

    private Menu createEditMenu()
    {
        Menu editMenu = new Menu("Edit");
        return editMenu;
    }

    private Menu createFormatMenu()
    {
        Menu formatMenu = new Menu("Format");
        return formatMenu;
    }

    private Menu createRunMenu()
    {
        Menu runMenu = new Menu("Run");
        MenuItem runProgram = new MenuItem("Run Program");
        runMenu.getItems().addAll(runProgram);

        runProgram.setOnAction(e -> {
            String input = this.inputBox.getTextArea().getText();
            SourceInput sourceInput = new SourceInput(input);
            Compiler compiler = new Compiler();
            SourceOutput output = compiler.compile(sourceInput);

            this.outputBox.getTextFlow().getChildren().clear();
            this.outputBox.getTextFlow().getChildren().addAll(output.getResult());
        });

        return runMenu;
    }

    private Menu createOptionsMenu()
    {
        Menu optionsMenu = new Menu("Options");
        return optionsMenu;
    }

    private Menu createHelpMenu()
    {
        Menu helpMenu = new Menu("Help");
        return helpMenu;
    }
}
