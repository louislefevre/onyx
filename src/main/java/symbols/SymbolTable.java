package symbols;

import java.util.HashMap;

public final class SymbolTable
{
    private final HashMap<String, Object> variables;

    public SymbolTable()
    {
        this.variables = new HashMap<>();
    }

    public HashMap<String, Object> getVariables()
    {
        return variables;
    }
}
