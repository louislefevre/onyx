package ui.main;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.text.TextFlow;
import javafx.stage.Stage;
import org.fxmisc.richtext.CodeArea;
import ui.StageManager;

import java.io.IOException;

public final class MainController
{
    @FXML private CodeArea codeAreaInput;
    @FXML private TextFlow textFlowOutput;
    @FXML private Label lineInfoLabel;
    @FXML private TextField tabSizeField;

    private AlertManager alertManager;
    private FileManager fileManager;
    private CodeAreaManager codeAreaManager;

    @FXML
    void initialize()
    {
        alertManager = new AlertManager();
        fileManager = new FileManager();
        codeAreaManager = new CodeAreaManager(codeAreaInput, tabSizeField, 4);
    }

    @FXML
    void runSource()
    {
        TextFlow output = codeAreaManager.compileInput();
        textFlowOutput.getChildren().clear();
        textFlowOutput.getChildren().addAll(output);
    }

    @FXML
    void openRepl() throws IOException
    {
        StageManager stageManager = new StageManager();
        stageManager.startRepl(new Stage());
    }

    @FXML
    void updateLineInfo()
    {
        String info = codeAreaManager.getCaretPosition();
        lineInfoLabel.setText(info);
    }

    @FXML
    void updateTabSize()
    {
        codeAreaManager.refreshTabSize();
    }

    @FXML
    void limitInputSize()
    {
        codeAreaManager.limitTabSize(2);
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
    void exitProgram()
    {
        String text = codeAreaInput.getText();
        if (containsUnsavedWork(text))
            return;

        Platform.exit();
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
