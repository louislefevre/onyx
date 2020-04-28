package analysis.lexical;

public enum Syntax
{
    AND("AND"),
    CARET("^"),
    CLOSE_BRACE("}"),
    CLOSE_PARENTHESIS(")"),
    DECIMAL_POINT("."),
    DOUBLE_QUOTES("\""),
    EOF("\0"),
    EQUALS("="),
    EQUALS_EQUALS("=="),
    FALSE("false"),
    GREATER(">"),
    GREATER_EQUALS(">="),
    HASH("#"),
    LESS("<"),
    LESS_EQUALS("<="),
    MINUS("-"),
    NOT("!"),
    NOT_EQUALS("!="),
    OPEN_BRACE("{"),
    OPEN_PARENTHESIS("("),
    OR("OR"),
    PERCENT("%"),
    PLUS("+"),
    SLASH("/"),
    STAR("*"),
    TRUE("true");

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
