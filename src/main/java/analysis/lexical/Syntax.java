package analysis.lexical;

public enum Syntax
{
    // Data Types
    DECIMAL_POINT("."),
    TRUE("true"),
    FALSE("false"),
    DOUBLE_QUOTES("\""),

    // Keywords
    IF("if"),
    ELSE("else"),

    // Separators
    OPEN_BRACE("{"),
    CLOSE_BRACE("}"),
    OPEN_PARENTHESIS("("),
    CLOSE_PARENTHESIS(")"),

    // Unary Operators
    NOT("!"),

    // Mathematical Binary Operators
    PLUS("+"),
    MINUS("-"),
    STAR("*"),
    SLASH("/"),
    PERCENT("%"),
    CARET("^"),

    // Conditional Binary Operators
    AND("AND"),
    OR("OR"),
    GREATER(">"),
    GREATER_EQUALS(">="),
    LESS("<"),
    LESS_EQUALS("<="),
    EQUALS_EQUALS("=="),

    // Assignment Operators
    EQUALS("="),
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
