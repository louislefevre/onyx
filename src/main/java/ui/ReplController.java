package ui;

import compilation.Pipeline;
import javafx.fxml.FXML;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.text.Text;
import source.SourceOutput;

import java.util.LinkedList;

public final class ReplController
{
    @FXML private TableView<TableManager.SymbolElement> symbolTableView;
    @FXML private TableColumn<TableManager.SymbolElement, String> symbolNamesColumn;
    @FXML private TableColumn<TableManager.SymbolElement, String> symbolTypesColumn;
    @FXML private TableColumn<TableManager.SymbolElement, String> symbolValuesColumn;

    @FXML private TextField inputField;
    @FXML private Text resultText;
    @FXML private Text historyText;

    private Pipeline pipeline;
    private LinkedList<String> historyLines;
    private TableManager tableManager;

    @FXML
    void initialize()
    {
        pipeline = new Pipeline();
        historyLines = new LinkedList<>();
        tableManager = new TableManager(symbolTableView);

        tableManager.addColumn(symbolNamesColumn, "name");
        tableManager.addColumn(symbolTypesColumn, "type");
        tableManager.addColumn(symbolValuesColumn, "value");

        pipeline.enableReplMode();
    }

    @FXML
    void evaluateInput()
    {
        String output = getResult();

        if (output == null)
            return;

        resultText.setText(output);
        tableManager.refreshTable(pipeline.getSymbolTable());
        inputField.clear();
        addToHistory(output);
    }

    @FXML
    void clearFields()
    {
        pipeline.getSymbolTable().clearSymbolTable();
        historyLines.clear();
        tableManager.clearTable();
        inputField.clear();
        resultText.setText("");
        historyText.setText("");
    }

    private void addToHistory(String output)
    {
        StringBuilder historyBuilder = new StringBuilder();
        historyLines.forEach(historyBuilder::append);
        historyText.setText(historyBuilder.toString());

        String text = output + System.getProperty("line.separator");
        historyLines.addFirst(text);
    }

    private String getResult()
    {
        String input = inputField.getText();

        if (input.isBlank())
            return null;

        SourceOutput sourceOutput = pipeline.compile(input);
        return sourceOutput.getOutput().toString();
    }
}
