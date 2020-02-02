package main.java.codeanalysis;

public class Utilities
{
    private Utilities() { }

    public static boolean isWhiteSpace(String str)
    {
        if(str.trim().isEmpty())
            return true;
        return false;
    }

    public static boolean isNull(Object object)
    {
        if(object == null)
            return true;
        return false;
    }

    public static boolean isParsable(String str)
    {
        try {
            Integer.parseInt(str);
            return true;
        } catch(NumberFormatException error) {
            return false;
        }
    }
}
