package ui;

import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import symbols.SymbolTable;

final class TableManager
{
    private final TableView<SymbolElement> table;

    public TableManager(TableView<SymbolElement> table)
    {
        this.table = table;
    }

    public void addColumn(TableColumn<SymbolElement, String> column, String fieldName)
    {
        column.setCellValueFactory(new PropertyValueFactory<>(fieldName));
    }

    public void refreshTable(SymbolTable symbolTable)
    {
        ObservableList<SymbolElement> symbols = FXCollections.observableArrayList();
        symbolTable.getSymbols().forEach((k, v) -> {
            symbols.add(new SymbolElement(k, v.getType().toString(), v.getValue().toString()));
        });

        table.setItems(symbols);
    }

    public void clearTable()
    {
        table.getItems().clear();
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
