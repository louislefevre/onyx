package main.java.codeanalysis;

public class Utilities
{
    private Utilities() { }

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
