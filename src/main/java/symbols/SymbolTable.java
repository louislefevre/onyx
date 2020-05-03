package symbols;

import identifiers.ObjectType;
import org.jetbrains.annotations.TestOnly;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public final class SymbolTable
{
    private final HashMap<String, Symbol> symbols;

    public SymbolTable()
    {
        this.symbols = new HashMap<>();
    }

    public boolean containsSymbol(String name)
    {
        return this.symbols.containsKey(name);
    }

    public Symbol getSymbol(String name)
    {
        return this.symbols.get(name);
    }

    public void addSymbol(String name, Object value, ObjectType type)
    {
        Symbol symbol = new Symbol(name, value, type);
        this.symbols.put(name, symbol);
    }

    @TestOnly
    public void printSymbolTable()
    {
        List<List<String>> rows = new ArrayList<>();
        List<String> headers = Arrays.asList("Name", "Value", "Type");
        rows.add(headers);

        for (Symbol symbol : this.symbols.values())
            rows.add(Arrays.asList(symbol.getName(), symbol.getValue().toString(), symbol.getType().toString()));

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

        String frame = "-----------------------------------\n";

        return frame + result.toString() + frame;
    }
}

