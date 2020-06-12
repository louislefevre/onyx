package symbols;

import lombok.Getter;
import lombok.Setter;
import types.ObjectType;

/**
 * The Symbol class is used to store information about variables declared during compilation.
 * <p>
 * The name, value, and data type of a variable is stored in a Symbol object, which is then stored in an instance of
 * SymbolTable.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
@Setter
public final class Symbol
{
    private final String name;
    private Object value;
    private ObjectType type;

    /**
     * Constructs a Symbol object initialised with its name, value, and data type.
     *
     * @param name The name of the symbol
     * @param value The value of the symbol
     * @param type The data type of the symbol
     */
    public Symbol(String name, Object value, ObjectType type)
    {
        this.name = name;
        this.value = value;
        this.type = type;
    }

    /**
     * Constructs a Symbol object initialised with its name and data type.
     * <p>
     * The value field automatically defaults to an empty value based on the data type parameter:
     * - INTEGER_OBJECT is 0
     * - DOUBLE_OBJECT is 0.0
     * - BOOLEAN_OBJECT is false
     * - STRING_OBJECT is an empty String
     *
     * @param name The name of the symbol
     * @param type The data type of the symbol
     */
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
