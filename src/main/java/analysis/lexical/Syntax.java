package analysis.lexical;

public final class Syntax
{
    public static final String PLUS = "+";
    public static final String MINUS = "-";
    public static final String STAR = "*";
    public static final String SLASH = "/";
    public static final String CARET = "^";
    public static final String PERCENT = "%";

    public static final String OPEN_PARENTHESIS = "(";
    public static final String CLOSE_PARENTHESIS = ")";

    public static final String AMPERSAND = "&";
    public static final String AND = "&&";

    public static final String PIPE = "|";
    public static final String OR = "||";

    public static final String EQUALS = "=";
    public static final String EQUALS_EQUALS = "==";
    public static final String NOT = "!";
    public static final String NOT_EQUALS = "!=";

    public static final String GREATER = ">";
    public static final String LESS = "<";
    public static final String GREATER_EQUALS = ">=";
    public static final String LESS_EQUALS = "<=";

    public static final String TRUE = "true";
    public static final String FALSE = "false";

    public static final String ESCAPE = "\0";

    private Syntax()
    {
        // Prevents class instantiation
        throw new UnsupportedOperationException();
    }
}