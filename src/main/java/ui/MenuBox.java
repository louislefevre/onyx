package ui;

import compilation.Pipeline;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.MenuItem;
import lombok.Getter;
import source.SourceOutput;

import java.util.ArrayList;
import java.util.List;

@Getter
public final class MenuBox
{
    private final InputBox inputBox;
    private final OutputBox outputBox;

    public MenuBox(InputBox inputBox, OutputBox outputBox)
    {
        this.inputBox = inputBox;
        this.outputBox = outputBox;
    }

    public MenuBar getMenuBar()
    {
        MenuBar menuBar = new MenuBar();
        menuBar.getMenus().addAll(createFileMenu(), createEditMenu(), createFormatMenu(),
                                  createRunMenu(), createOptionsMenu(), createHelpMenu());
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
            String input = inputBox.getCodeArea().getText() + System.getProperty("line.separator");
            String[] lines = input.split(System.getProperty("line.separator"));

            List<String> linesList = new ArrayList<>();
            for (String line : lines)
                if (!line.isBlank())
                    linesList.add(line);

            if (linesList.isEmpty())
                return;

            Pipeline pipeline = new Pipeline();
            for (String line : linesList)
                if (!line.isBlank())
                    pipeline.compile(line);

            SourceOutput sourceOutput = pipeline.compile(input);

            if (sourceOutput.getResult() == null)
                return;

            outputBox.getTextFlow().getChildren().clear();
            outputBox.getTextFlow().getChildren().addAll(sourceOutput.getTextResult());
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
