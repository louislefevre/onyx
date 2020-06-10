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

    @FXML
    void clearFields()
    {
        inputField.clear();
        tableManager.clear();
        resultTextFlow.getChildren().clear();
        compiler.getSymbolTable().clear();
    }
}