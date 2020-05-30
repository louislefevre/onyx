package ui;

import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.text.TextFlow;
import org.fxmisc.richtext.CodeArea;
import source.SourceOutput;

import java.io.IOException;

public final class MainController
{
    @FXML private CodeArea codeAreaInput;
    @FXML private TextFlow textFlowOutput;
    @FXML private Label lineInfoLabel;
    @FXML private TextField tabSizeField;

    private StageManager stageManager;
    private AlertManager alertManager;
    private FileManager fileManager;
    private CodeManager codeManager;

    @FXML
    void initialize()
    {
        stageManager = new StageManager();
        alertManager = new AlertManager();
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
        textFlowOutput.getChildren().addAll(sourceOutput.getTextOutput());
    }

    @FXML
    void openRepl() throws IOException
    {
        stageManager.startReplStage();
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
    void newFile()
    {
        String text = codeAreaInput.getText();

        if (containsUnsavedWork(text))
            return;

        String fileText = fileManager.openNewFile();
        codeAreaInput.replaceText(fileText);
    }

    @FXML
    void openFileChooser()
    {
        try
        {
            String text = codeAreaInput.getText();

            if (containsUnsavedWork(text))
                return;

            String fileText = fileManager.openFile();
            codeAreaInput.replaceText(fileText);
        }
        catch (IOException | IllegalArgumentException exception)
        {
            openPopupAlert(exception.getMessage());
        }
        catch (NullPointerException exception)
        {
            System.out.println(exception.getMessage());
        }
    }

    @FXML
    void saveFile()
    {
        try
        {
            String text = codeAreaInput.getText();
            fileManager.saveFile(text);
        }
        catch (IOException | IllegalArgumentException exception)
        {
            openPopupAlert(exception.getMessage());
        }
        catch (NullPointerException exception)
        {
            System.out.println(exception.getMessage());
        }
    }

    @FXML
    void saveFileAs()
    {
        String text = codeAreaInput.getText();
        try
        {
            fileManager.saveFileAs(text);
        }
        catch (IOException | IllegalArgumentException exception)
        {
            openPopupAlert(exception.getMessage());
        }
        catch (NullPointerException exception)
        {
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

    private void openPopupAlert(String message)
    {
        alertManager.startErrorAlert(message);
    }

    private boolean openConfirmationAlert(String message)
    {
        return alertManager.startConfirmationAlert(message);
    }

    private boolean containsUnsavedWork(String text)
    {
        boolean saved = fileManager.checkIfSaved(text);

        if (saved)
            return false;

        String message = "You have unsaved changes that will be lost if you open another file.\n" +
                         "Are you sure you want to continue?";
        boolean confirmation = openConfirmationAlert(message);

        return !confirmation;
    }
}
