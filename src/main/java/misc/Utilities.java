package misc;

public final class Utilities
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

    public static int minimumZero(int num)
    {
        if(num <= 0)
            return 0;
        return num;
    }
}
