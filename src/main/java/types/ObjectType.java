package types;

/**
 * The ObjectType enum is used to store constants related to an Objects data type.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public enum ObjectType
{
    INTEGER_OBJECT,
    DOUBLE_OBJECT,
    BOOLEAN_OBJECT,
    STRING_OBJECT,
    NULL_OBJECT;

    /**
     * Returns a user friendly version of the data types name.
     * <p>
     * The name of the ObjectType enum is split by its underscore, with the left portion being retrieved and
     * converted to lowercase.
     *
     * @return The name of the data type
     */
    @Override
    public String toString()
    {
        // Split the enum by the underscore, take the left string, and convert to lowercase
        return this.name().split("_")[0].toLowerCase();
    }
}
