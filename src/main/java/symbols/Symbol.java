package symbols;

import lombok.Getter;
import lombok.Setter;
import types.ObjectType;

@Getter
public final class Symbol
{
    private final String name;
    @Setter private Object value;
    @Setter private ObjectType type;

    public Symbol(String name, Object value, ObjectType type)
    {
        this.name = name;
        this.value = value;
        this.type = type;
    }
}
