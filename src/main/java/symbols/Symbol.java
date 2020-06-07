package symbols;

import lombok.Getter;
import lombok.Setter;
import types.ObjectType;

@Getter
@Setter
public final class Symbol
{
    private final String name;
    private Object value;
    private ObjectType type;

    public Symbol(String name, Object value, ObjectType type)
    {
        this.name = name;
        this.value = value;
        this.type = type;
    }

    public Symbol(String name, ObjectType type)
    {

        this(name, getDefaultValue(type), type);
    }

    private static Object getDefaultValue(ObjectType type)
    {
        switch(type)
        {
            case INTEGER_OBJECT:
                return 0;
            case DOUBLE_OBJECT:
                return 0.0;
            case BOOLEAN_OBJECT:
                return false;
            case STRING_OBJECT:
                return "";
            default:
                return null;
        }
    }
}
