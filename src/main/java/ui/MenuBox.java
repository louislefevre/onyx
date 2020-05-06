package ui;

import compilation.Pipeline;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.MenuItem;
import javafx.scene.text.TextFlow;
import javafx.stage.Stage;
import lombok.Getter;
import org.fxmisc.richtext.CodeArea;
import source.SourceOutput;

import java.util.ArrayList;
import java.util.List;

@Getter
public final class MenuBox
{
    private final CodeArea inputArea;
    private final TextFlow outputArea;

    public MenuBox(IdeScene scene)
    {
        this.inputArea = scene.getInputArea();
        this.outputArea = scene.getOutputArea();
    }

    public MenuBar getMenuBar()
    {
        MenuBar menuBar = new MenuBar();
        menuBar.getMenus().addAll(createFileMenu(), createRunMenu(), createOptionsMenu());
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

    private Menu createRunMenu()
    {
        Menu runMenu = new Menu("Run");
        MenuItem runProgram = new MenuItem("Run Program");
        MenuItem runRepl = new MenuItem("Run REPL");
        runMenu.getItems().addAll(runProgram, runRepl);

        runProgram.setOnAction(e -> {
            runProgram();
        });

        runRepl.setOnAction(e -> {
            runRepl();
        });

        return runMenu;
    }

    private Menu createOptionsMenu()
    {
        Menu optionsMenu = new Menu("Options");
        return optionsMenu;
    }

    private void runRepl()
    {
        Stage stage = new Stage();
        ReplScene replScene = new ReplScene();
        replScene.setStage(stage);
    }

    private void runProgram()
    {
        outputArea.getChildren().clear();
        String input = inputArea.getText();

        if(input.isBlank())
            return;

        SourceOutput sourceOutput = readInput(input);

        if (sourceOutput.getResult() == null) // Avoids NullPointerException if input is invalid
            return;

        outputArea.getChildren().addAll(sourceOutput.getTextResult());
    }

    private SourceOutput readInput(String input)
    {
        input += System.getProperty("line.separator"); // Adds extra line separator at end to avoid collision with EOF
        input = input.replaceAll("\011", ""); // Ignores horizontal tabs, breaks line separators otherwise
        String[] lines = input.split(System.getProperty("line.separator")); // Splits each line up to be run individually

        List<String> linesList = new ArrayList<>();
        for (String line : lines)
            if (!line.isBlank()) // Only adds non-blank lines
                linesList.add(line);

        Pipeline pipeline = new Pipeline();
        for (String line : linesList)
            if (!line.isBlank())
                pipeline.compile(line);

        return pipeline.compile(input);
    }
}
