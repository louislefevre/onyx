package analysis.lexical;

public enum Syntax
{
    PLUS("+"),
    MINUS("-"),
    STAR("*"),
    SLASH("/"),
    CARET("^"),
    PERCENT("%"),
    OPEN_PARENTHESIS("("),
    CLOSE_PARENTHESIS(")"),
    EQUALS("="),
    EQUALS_EQUALS("=="),
    NOT("!"),
    NOT_EQUALS("!="),
    GREATER(">"),
    LESS("<"),
    GREATER_EQUALS(">="),
    LESS_EQUALS("<="),
    AND("AND"),
    OR("OR"),
    TRUE("true"),
    FALSE("false"),
    EOF("\0");

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
