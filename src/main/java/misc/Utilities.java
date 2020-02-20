package misc;

public final class Utilities
{
    private Utilities()
    {
        // Prevents class instantiation
        throw new UnsupportedOperationException();
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

    public static int minimumZero(int num)
    {
        return Math.max(num, 0);
    }
}
