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
    VAR("var"),

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
    LESS("<"),
    GREATER_EQUALS(">="),
    LESS_EQUALS("<="),
    NOT("!"),
    EQUALS("="),
    EQUALS_EQUALS("=="),
    NOT_EQUALS("!="),

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
