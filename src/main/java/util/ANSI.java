package util;

public final class ANSI
{
    public static final String RESET = "\u001B[0m";
    public static final String RED = "\u001B[31m";
    public static final String GREY = "\u001B[37m";

    private ANSI()
    {
        // Prevents class instantiation
        throw new UnsupportedOperationException();
    }
}
