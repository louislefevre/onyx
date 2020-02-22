package symbols;

public final class Symbols
{
    public static final String PLUS = "+";
    public static final String MINUS = "-";
    public static final String STAR = "*";
    public static final String SLASH = "/";

    public static final String OPEN_PARENTHESIS = "(";
    public static final String CLOSE_PARENTHESIS = ")";

    public static final String AMPERSAND = "&";
    public static final String AND = "&&";

    public static final String PIPE = "|";
    public static final String OR = "||";

    public static final String ASSIGN = "=";
    public static final String EQUALS = "==";
    public static final String BANG = "!";
    public static final String NOT_EQUALS = "!=";

    public static final String TRUE = "true";
    public static final String FALSE = "false";

    public static final String ESCAPE = "\0";

    private Symbols()
    {
        // Prevents class instantiation
        throw new UnsupportedOperationException();
    }
}
