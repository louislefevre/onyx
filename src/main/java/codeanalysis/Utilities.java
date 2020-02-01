package main.java.codeanalysis;

public class Utilities
{
    public static void Utilities()
    {

    }

    public static boolean isWhiteSpace(String string)
    {
        if(string.trim().isEmpty())
            return true;
        return false;
    }

    public static boolean isNull(Object object)
    {
        if(object == null)
            return true;
        return false;
    }
}
