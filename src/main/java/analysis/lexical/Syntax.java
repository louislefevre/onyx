package analysis.lexical;

public enum Syntax
{
    // Data Syntax
    DECIMAL_POINT("."),
    DOUBLE_QUOTES("\""),

    // Keyword Syntax
    TRUE("true"),
    FALSE("false"),
    AND("AND"),
    OR("OR"),

    // Separator Syntax
    OPEN_BRACE("{"),
    CLOSE_BRACE("}"),
    OPEN_PARENTHESIS("("),
    CLOSE_PARENTHESIS(")"),

    // Operator Syntax
    PLUS("+"),
    MINUS("-"),
    STAR("*"),
    SLASH("/"),
    PERCENT("%"),
    CARET("^"),
    GREATER(">"),
    GREATER_EQUALS(">="),
    LESS("<"),
    LESS_EQUALS("<="),
    EQUALS("="),
    EQUALS_EQUALS("=="),
    NOT("!"),
    NOT_EQUALS("!="),
    PLUS_EQUALS("+="),
    MINUS_EQUALS("-="),
    STAR_EQUALS("*="),
    SLASH_EQUALS("/="),
    PERCENT_EQUALS("%="),
    CARET_EQUALS("^="),

    // Break Syntax
    EOF("\0"),

    // Not-parsed Syntax
    HASH("#");

    private final String syntax;

    Syntax(String syntax)
    {
        this.syntax = syntax;
    }

    public String getSyntax()
    {
        return this.syntax;
    }
}
