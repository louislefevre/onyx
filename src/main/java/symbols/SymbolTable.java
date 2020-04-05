package symbols;

import identifiers.ObjectType;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.ArrayList;
import java.util.List;

public final class SymbolTable
{
    private final List<Symbol> symbols;

    public SymbolTable()
    {
        this.symbols = new ArrayList<>();
    }

    public boolean containsSymbol(String name)
    {
        return this.getSymbol(name) != null;
    }

    public void addSymbol(String name, Object value, ObjectType type)
    {
        if (this.containsSymbol(name))
        {
            Symbol symbol = this.getSymbol(name);
            symbol.setValue(value);
            symbol.setType(type);
            return;
        }
        Symbol symbol = new Symbol(name, value, type);
        this.symbols.add(symbol);
    }

    @Nullable
    public Symbol getSymbol(String name)
    {
        for (Symbol symbol : this.symbols)
            if (symbol.getName().equals(name))
                return symbol;

        return null;
    }

    @TestOnly
    public void printSymbolTable()
    {
        System.out.println("----------------------------");
        for (Symbol symbol : this.symbols)
        {
            String message = String.format("%1s   %2s   %3s", symbol.getName(), symbol.getValue(), symbol.getType());
            System.out.println(message);
        }
        System.out.println("----------------------------");
    }
}
