package ui;

import compilation.Pipeline;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.text.Text;
import source.SourceOutput;

import java.util.LinkedList;

public class ReplController
{
    @FXML private TableView<SymbolElement> symbolTable;
    @FXML private TableColumn<SymbolElement, String> symbolNamesColumn;
    @FXML private TableColumn<SymbolElement, String>  symbolTypesColumn;
    @FXML private TableColumn<SymbolElement, String>  symbolValuesColumn;

    @FXML private TextField inputField;
    @FXML private Text resultText;
    @FXML private Text historyText;

    @FXML
    void initialize()
    {
        symbolNamesColumn.setCellValueFactory(new PropertyValueFactory<>("name"));
        symbolTypesColumn.setCellValueFactory(new PropertyValueFactory<>("type"));
        symbolValuesColumn.setCellValueFactory(new PropertyValueFactory<>("value"));

        pipeline.enableReplMode();
    }

    private LinkedList<String> lines = new LinkedList<>();
    private Pipeline pipeline = new Pipeline();

    @FXML
    void evaluateInput()
    {
        String input = inputField.getText();
        if(input.isBlank())
            return;

        SourceOutput sourceOutput = pipeline.compile(input);
        String output = sourceOutput.getSimpleResult().toString();

        resultText.setText(output);

        StringBuilder historyBuilder = new StringBuilder();
        lines.forEach(historyBuilder::append);
        historyText.setText(historyBuilder.toString());

        String text = output + System.getProperty("line.separator");
        lines.addFirst(text);

        StringBuilder symbolBuilder = new StringBuilder();
        pipeline.getSymbolTable().getSymbols().forEach((k,v) -> {
            String symbol = String.format("%1s = %2s (%3s)", k, v.getValue(), v.getType());
            symbolBuilder.append(symbol);
            symbolBuilder.append(System.getProperty("line.separator"));
        });

        ObservableList<SymbolElement> symbols = FXCollections.observableArrayList();
        pipeline.getSymbolTable().getSymbols().forEach((k, v) -> {
            symbols.add(new SymbolElement(k, v.getType().toString(), v.getValue().toString()));
        });

        symbolTable.setItems(symbols);

        inputField.clear();
    }

    @FXML
    void clearFields()
    {
        pipeline.getSymbolTable().clearSymbolTable();
        symbolTable.getItems().clear();
        lines.clear();
        inputField.clear();
        resultText.setText("");
        historyText.setText("");
    }

    public class SymbolElement
    {
        private final SimpleStringProperty name;
        private final SimpleStringProperty type;
        private final SimpleStringProperty value;

        public SymbolElement(String name, String type, String value)
        {
            this.name = new SimpleStringProperty(name);
            this.type = new SimpleStringProperty(type);
            this.value = new SimpleStringProperty(value);
        }

        public String getName()
        {
            return name.get();
        }

        public String getType()
        {
            return type.get();
        }

        public String getValue()
        {
            return value.get();
        }

        public void setName(String name)
        {
            this.name.set(name);
        }

        public void setType(String type)
        {
            this.type.set(type);
        }

        public void setValue(String value)
        {
            this.value.set(value);
        }
    }
}
