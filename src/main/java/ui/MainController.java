package ui;

import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import org.fxmisc.richtext.CodeArea;
import source.SourceOutput;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

public final class MainController
{
    @FXML private CodeArea codeAreaInput;
    @FXML private TextFlow textFlowOutput;
    @FXML private Label lineInfoLabel;
    @FXML private TextField tabSizeField;

    private SceneManager sceneManager;
    private FileManager fileManager;
    private CodeManager codeManager;

    @FXML
    void initialize()
    {
        sceneManager = new SceneManager();
        fileManager = new FileManager();
        codeManager = new CodeManager(codeAreaInput, tabSizeField, 4);
    }

    @FXML
    void runSource()
    {
        textFlowOutput.getChildren().clear();
        String input = codeManager.getCodeInput();

        if (input.isBlank())
            return;

        SourceOutput sourceOutput = codeManager.readInput(input);

        List<Text> result = sourceOutput.getTextOutput();
        if (result == null) // Avoids NullPointerException if input is invalid
            return;

        textFlowOutput.getChildren().addAll(result);
    }

    @FXML
    void openRepl() throws IOException
    {
        sceneManager.startReplStage();
    }

    @FXML
    void updateLineInfo()
    {
        String info = codeManager.getLineInfo();
        lineInfoLabel.setText(info);
    }

    @FXML
    void updateTabSize()
    {
        codeManager.refreshTabSize();
    }

    @FXML
    void limitInputSize()
    {
        codeManager.limitTabSize();
    }

    @FXML
    void openFileChooser()
    {
        try {
            String fileText = fileManager.openFile();
            codeAreaInput.replaceText(fileText);
        }
        catch (IOException | IllegalArgumentException exception) {
            openPopupWindow(exception.getMessage());
        }
        catch (NullPointerException exception) {
            System.out.println(exception.getMessage());
        }
    }

    @FXML
    void saveFile()
    {
        try {
            String text = codeAreaInput.getText();
            fileManager.saveFile(text);
        }
        catch (IOException | IllegalArgumentException exception) {
            openPopupWindow(exception.getMessage());
        }
        catch (NullPointerException exception) {
            System.out.println(exception.getMessage());
        }
    }

    @FXML
    void saveFileAs()
    {
        String text = codeAreaInput.getText();
        try {
            fileManager.saveFileAs(text);
        }
        catch (IOException | IllegalArgumentException exception) {
            openPopupWindow(exception.getMessage());
        }
        catch (NullPointerException exception) {
            System.out.println(exception.getMessage());
        }
    }

    @FXML
    void openHelpInfo() throws IOException
    {
        // Only works on Linux
        String url = "https://github.com/louislefevre/onyx-compiler";
        Runtime runtime = Runtime.getRuntime();
        runtime.exec("xdg-open " + url);
    }

    private void openPopupWindow(String message)
    {
        sceneManager.startPopupStage(message);
    }
}
