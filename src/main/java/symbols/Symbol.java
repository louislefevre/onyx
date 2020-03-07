package symbols;

public final class Symbol
{
    private String name;
    private Object value;
    private Object type;

    public Symbol(String name, Object value, Object type)
    {
        this.name = name;
        this.value = value;
        this.type = type;
    }
}
