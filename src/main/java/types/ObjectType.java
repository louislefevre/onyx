package types;

public enum ObjectType
{
    INTEGER_OBJECT,
    DOUBLE_OBJECT,
    BOOLEAN_OBJECT,
    STRING_OBJECT,
    NULL_OBJECT;

    @Override
    public String toString()
    {
        // Split the enum by the underscore, take the left string, and convert to lowercase
        return this.name().split("_")[0].toLowerCase();
    }
}
