package ui.repl;

import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import symbols.SymbolTable;

/**
 * The TableManager class is used manage the TableView representing the Symbols contained within the SymbolTable.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
final class TableManager
{
    private final TableView<SymbolElement> table;
    private final SymbolTable symbolTable;

    /**
     * Constructs a TableManager object for storing a TableView and SymbolTable.
     * <p>
     * The TableView is used to store each SymbolElement object as rows, whilst the SymbolTable is used to
     * retrieve the Symbols.
     *
     * @param table The TableView for storing the Symbols
     * @param symbolTable The SymbolTable for retrieving the Symbols
     */
    public TableManager(TableView<SymbolElement> table, SymbolTable symbolTable)
    {
        this.table = table;
        this.symbolTable = symbolTable;
    }

    /**
     * Adds a column to be displayed in the table.
     * <p>
     * Each TableColumn must accept a SymbolElement and a String, and also provide a name for the column.
     *
     * @param column A TableColumn The TableColumn to be added to the table
     * @param fieldName The name of the column
     */
    public void addColumn(TableColumn<SymbolElement, String> column, String fieldName)
    {
        column.setCellValueFactory(new PropertyValueFactory<>(fieldName));
    }

    /**
     * Refreshes the table to reflect the contents of the SymbolTable.
     * <p>
     * This method is called every time the SymbolTable is updated in some way, whether a Symbol is added, removed,
     * or updated.
     */
    public void refresh()
    {
        ObservableList<SymbolElement> symbols = FXCollections.observableArrayList();
        symbolTable.getSymbols().forEach((k, v) -> {
            symbols.add(new SymbolElement(k, v.getType().toString(), v.getValue().toString()));
        });

        table.setItems(symbols);
    }

    /**
     * Clears the TableView, removing all TableColumn objects.
     */
    public void clear()
    {
        table.getItems().clear();
    }

    /**
     * The SymbolElement class is used store Symbols as rows within a TableView
     *
     * @author Louis Lefevre
     * @version 1.0
     * @since 1.0
     */
    public static class SymbolElement
    {
        private final SimpleStringProperty name;
        private final SimpleStringProperty type;
        private final SimpleStringProperty value;

        private SymbolElement(String name, String type, String value)
        {
            this.name = new SimpleStringProperty(name);
            this.type = new SimpleStringProperty(type);
            this.value = new SimpleStringProperty(value);
        }

        /**
         * Return the name of the Symbol
         *
         * @return the Symbol name
         */
        public String getName()
        {
            return name.get();
        }

        /**
         * Return the type of the Symbol
         *
         * @return the Symbol type
         */
        public String getType()
        {
            return type.get();
        }

        /**
         * Return the value of the Symbol
         *
         * @return the Symbol value
         */
        public String getValue()
        {
            return value.get();
        }

        /**
         * Set the name of the Symbol
         *
         * @param name The Symbol name
         */
        public void setName(String name)
        {
            this.name.set(name);
        }

        /**
         * Set the type of the Symbol
         *
         * @param type The Symbol type
         */
        public void setType(String type)
        {
            this.type.set(type);
        }

        /**
         * Set the value of the Symbol
         *
         * @param value The Symbol value
         */
        public void setValue(String value)
        {
            this.value.set(value);
        }
    }
}
