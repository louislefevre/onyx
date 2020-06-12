package ui.repl;

import compilation.Compiler;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import source.SourceOutput;

/**
 * The ReplController class is used communicate with and control the REPL GUI.
 * <p>
 * Communication is done through FXML, through the use of labels in the FXML file and annotations in this Java class.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class ReplController
{
    @FXML private TableView<TableManager.SymbolElement> symbolTableView;
    @FXML private TableColumn<TableManager.SymbolElement, String> symbolNamesColumn;
    @FXML private TableColumn<TableManager.SymbolElement, String> symbolTypesColumn;
    @FXML private TableColumn<TableManager.SymbolElement, String> symbolValuesColumn;

    @FXML private TextField inputField;
    @FXML private TextFlow resultTextFlow;

    private Compiler compiler;
    private TableManager tableManager;

    /**
     * Runs when the REPL is started up.
     * <p>
     * Is used to initialise the Compiler and TableView so that they are ready for use.
     */
    @FXML
    void initialize()
    {
        compiler = new Compiler();
        compiler.setReplMode(true);

        symbolTableView.setPlaceholder(new Label());
        tableManager = new TableManager(symbolTableView, compiler.getSymbolTable());
        tableManager.addColumn(symbolNamesColumn, "name");
        tableManager.addColumn(symbolTypesColumn, "type");
        tableManager.addColumn(symbolValuesColumn, "value");
    }

    /**
     * Evaluates the users input and displays the output.
     * <p>
     * The input given by the user is extracted from the TextField, and then compiled by the Compiler. The result
     * is then added to the TextFlow to be displayed.
     */
    @FXML
    void evaluateInput()
    {
        String input = inputField.getText();
        if (input.isBlank())
            return;

        SourceOutput sourceOutput = compiler.compile(input);
        resultTextFlow.getChildren().add(0, sourceOutput.getOutput());
        resultTextFlow.getChildren().add(1, new Text(System.lineSeparator()));

        tableManager.refresh();
        inputField.clear();
    }

    /**
     * Resets the REPL to its start state.
     * <p>
     * The input TextField, TableView, output TextFlow, and SymbolTable are all reset.
     */
    @FXML
    void clearFields()
    {
        inputField.clear();
        tableManager.clear();
        resultTextFlow.getChildren().clear();
        compiler.getSymbolTable().clear();
    }
}
