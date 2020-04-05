package util;

import identifiers.ObjectType;
import org.jetbrains.annotations.NotNull;

public final class Utilities
{
    private Utilities()
    {
        // Prevents class instantiation
        throw new UnsupportedOperationException();
    }

    public static boolean isParsable(String str)
    {
        try
        {
            Integer.parseInt(str);
            return true;
        } catch (NumberFormatException error)
        {
            return false;
        }
    }

    public static int minimumZero(int num)
    {
        return Math.max(num, 0);
    }

    public static boolean isWhitespace(@NotNull String str)
    {
        return str.isBlank();
    }

    public static boolean isLetter(@NotNull String str)
    {
        if (str.length() != 1)
            return false;
        return Character.isLetter(str.charAt(0));
    }

    public static boolean isDigit(@NotNull String str)
    {
        if (str.length() != 1)
            return false;
        return Character.isDigit(str.charAt(0));
    }

    public static ObjectType typeOf(Object object)
    {
        if (object instanceof Integer)
            return ObjectType.INTEGER_OBJECT;
        else if (object instanceof Boolean)
            return ObjectType.BOOLEAN_OBJECT;
        else
            return ObjectType.NULL_OBJECT;
    }
}
