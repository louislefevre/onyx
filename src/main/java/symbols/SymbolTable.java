package symbols;

import identifiers.ObjectType;
import org.jetbrains.annotations.TestOnly;

import java.util.HashMap;

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
        this.addSymbol(symbol);
    }

    public void addSymbol(Symbol symbol)
    {
        this.symbols.put(symbol.getName(), symbol);
    }

    @TestOnly
    public void printSymbolTable()
    {
        System.out.println("----------------------------");
        for (Symbol symbol : this.symbols.values())
        {
            String message = String.format("%1s   %2s   %3s", symbol.getName(), symbol.getValue(), symbol.getType());
            System.out.println(message);
        }
        System.out.println("----------------------------");
    }
}
