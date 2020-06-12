package symbols;

import lombok.Getter;
import types.ObjectType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * The SymbolTable class is used to store Symbol objects that have been generated during compilation.
 * <p>
 * As compilation proceeds variables may be declared, which results in the creation of Symbol Objects. These are
 * passed to an instance of SymbolTable and stored within an internal HashMap, from which the Symbols can be
 * retrieved.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class SymbolTable
{
    private final HashMap<String, Symbol> symbols;

    /**
     * Constructs a SymbolTable object for storing Symbols that are declared during compilation.
     * <p>
     * A HashMap is initialised on construction for storing the Symbol objects.
     */
    public SymbolTable()
    {
        this.symbols = new HashMap<>();
    }

    /**
     * Returns a boolean indicating whether the SymbolTable holds a particular Symbol.
     * <p>
     * If true, the SymbolTable contains the specified Symbol. If false, the Symbol is not present.
     *
     * @param name A String identifying the Symbol by name
     * @return A boolean indicating if the SymbolTable contains a particular symbol
     */
    public boolean contains(String name)
    {
        return symbols.containsKey(name);
    }

    /**
     * Returns a Symbol contained within the SymbolTable.
     * <p>
     * Returns null if the symbol does not exist within the SymbolTable.
     *
     * @param name A String identifying the Symbol by name
     * @return The specified Symbol object
     */
    public Symbol get(String name)
    {
        return symbols.get(name);
    }

    /**
     * Add a Symbol object to the SymbolTable.
     *
     * @param symbol The symbol to be added
     */
    public void add(Symbol symbol)
    {
        add(symbol.getName(), symbol.getValue(), symbol.getType());
    }

    /**
     * Generate and add a Symbol object to the SymbolTable.
     * <p>
     * From the parameters, a Symbol object is generated and added to the SymbolTable.
     * <p>
     * If a Symbol with the same name already exists within the SymbolTable, its value and type are updated.
     *
     * @param name The name of the symbol
     * @param value The value of the symbol
     * @param type The type of the symbol
     */
    public void add(String name, Object value, ObjectType type)
    {
        if (contains(name))
        {
            Symbol symbol = get(name);
            symbol.setValue(value);
            symbol.setType(type);
            return;
        }
        Symbol symbol = new Symbol(name, value, type);
        symbols.put(name, symbol);
    }

    /**
     * Remove a Symbol from the SymbolTable.
     *
     * @param symbol The symbol to be removed
     */
    public void remove(Symbol symbol)
    {
        remove(symbol.getName());
    }

    /**
     * Remove a Symbol from the SymbolTable by its name.
     *
     * @param name The name of the symbol
     */
    public void remove(String name)
    {
        symbols.remove(name);
    }

    /**
     * Clears the SymbolTable, removing all stored Symbol objects.
     */
    public void clear()
    {
        symbols.clear();
    }

    /**
     * Pretty prints the SymbolTable to the console.
     */
    public void print()
    {
        if (symbols.isEmpty())
            return;

        List<List<String>> rows = new ArrayList<>();
        List<String> headers = Arrays.asList("Name", "Type", "Value");
        rows.add(headers);

        for (Symbol symbol : symbols.values())
        {
            String name = symbol.getName();
            String type = symbol.getType().name();
            String value = symbol.getValue() == null ? "null" : symbol.getValue().toString();
            rows.add(Arrays.asList(name, type, value));
        }

        System.out.print(formatAsTable(rows));
    }

    private static String formatAsTable(List<List<String>> rows)
    {
        // Sourced from: https://stackoverflow.com/a/50649715/7774790
        int[] maxLengths = new int[rows.get(0).size()];
        for (List<String> row : rows)
            for (int i = 0; i < row.size(); i++)
                maxLengths[i] = Math.max(maxLengths[i], row.get(i).length());

        StringBuilder formatBuilder = new StringBuilder();
        for (int maxLength : maxLengths)
            formatBuilder.append("%-").append(maxLength + 2).append("s");

        String format = formatBuilder.toString();
        StringBuilder result = new StringBuilder();
        for (List<String> row : rows)
        {
            Object[] element = row.toArray(new String[0]);
            result.append(String.format(format, element)).append("\n");
        }

        return result.toString();
    }
}

