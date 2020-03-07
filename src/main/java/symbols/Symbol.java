package symbols;

import identifiers.ObjectType;
import lombok.Getter;
import lombok.Setter;

public final class Symbol
{
    @Getter private String name;
    @Getter @Setter private Object value;
    @Getter @Setter private ObjectType type;

    public Symbol(String name, Object value, ObjectType type)
    {
        this.name = name;
        this.value = value;
        this.type = type;
    }
}
