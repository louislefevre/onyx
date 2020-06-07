package symbols;

import lombok.Getter;
import types.ObjectType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

@Getter
public final class SymbolTable
{
    private final HashMap<String, Symbol> symbols;

    public SymbolTable()
    {
        this.symbols = new HashMap<>();
    }

    public boolean contains(String name)
    {
        return symbols.containsKey(name);
    }

    public Symbol get(String name)
    {
        return symbols.get(name);
    }

    public void add(Symbol symbol)
    {
        add(symbol.getName(), symbol.getValue(), symbol.getType());
    }

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

    public void remove(Symbol symbol)
    {
        remove(symbol.getName());
    }

    public void remove(String name)
    {
        symbols.remove(name);
    }

    public void clear()
    {
        symbols.clear();
    }

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

