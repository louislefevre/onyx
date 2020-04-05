package analysis.lexical;

enum Syntax
{
    PLUS("+"),
    MINUS("-"),
    STAR("*"),
    SLASH("/"),
    CARET("^"),
    PERCENT("%"),
    OPEN_PARENTHESIS("("),
    CLOSE_PARENTHESIS(")"),

    AMPERSAND("&"),
    AND("&&"),

    PIPE("|"),
    OR("||"),

    EQUALS("="),
    EQUALS_EQUALS("=="),
    NOT("!"),
    NOT_EQUALS("!="),

    GREATER(">"),
    LESS("<"),
    GREATER_EQUALS(">="),
    LESS_EQUALS("<="),

    TRUE("true"),
    FALSE("false"),

    ESCAPE("\0");

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
