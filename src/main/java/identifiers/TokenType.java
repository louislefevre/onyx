package identifiers;

public enum TokenType
{
    // Data Types
    INTEGER_TOKEN,
    DOUBLE_TOKEN,
    BOOLEAN_TOKEN,
    STRING_TOKEN,
    IDENTIFIER_TOKEN,

    // Reserved Words
    IF_TOKEN,
    ELSE_TOKEN,
    LOOP_TOKEN,
    TO_TOKEN,
    AND_TOKEN,
    OR_TOKEN,

    // Separators
    OPEN_BRACE_TOKEN,
    CLOSE_BRACE_TOKEN,
    OPEN_PARENTHESIS_TOKEN,
    CLOSE_PARENTHESIS_TOKEN,

    // Unary Operators
    NOT_TOKEN,

    // Mathematical Binary Operators
    PLUS_TOKEN,
    MINUS_TOKEN,
    STAR_TOKEN,
    SLASH_TOKEN,
    PERCENT_TOKEN,
    CARET_TOKEN,

    // Conditional Binary Operators
    GREATER_TOKEN,
    GREATER_EQUALS_TOKEN,
    LESS_TOKEN,
    LESS_EQUALS_TOKEN,
    EQUALS_EQUALS_TOKEN,

    // Assignment Operators
    EQUALS_TOKEN,
    NOT_EQUALS_TOKEN,
    PLUS_EQUALS_TOKEN,
    MINUS_EQUALS_TOKEN,
    STAR_EQUALS_TOKEN,
    SLASH_EQUALS_TOKEN,
    PERCENT_EQUALS_TOKEN,
    CARET_EQUALS_TOKEN,

    // Structure Tokens
    LINE_BREAK_TOKEN,
    EOF_TOKEN,

    // Not-parsed Tokens
    BAD_TOKEN,
    COMMENT_TOKEN,
    WHITE_SPACE_TOKEN
}
