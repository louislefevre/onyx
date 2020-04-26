package identifiers;

public enum TokenType
{
    // Primary Tokens
    DOUBLE_TOKEN,
    IDENTIFIER_KEYWORD_TOKEN,
    INTEGER_TOKEN,
    STRING_TOKEN,

    // Keywords
    AND_TOKEN,
    FALSE_KEYWORD_TOKEN,
    OR_TOKEN,
    TRUE_KEYWORD_TOKEN,

    // Separator Tokens
    OPEN_PARENTHESIS_TOKEN,
    CLOSE_PARENTHESIS_TOKEN,

    // Operator Tokens
    CARET_TOKEN,
    EQUALS_EQUALS_TOKEN,
    EQUALS_TOKEN,
    GREATER_EQUALS_TOKEN,
    GREATER_TOKEN,
    LESS_EQUALS_TOKEN,
    LESS_TOKEN,
    MINUS_TOKEN,
    NOT_EQUALS_TOKEN,
    NOT_TOKEN,
    PERCENT_TOKEN,
    PLUS_TOKEN,
    SLASH_TOKEN,
    STAR_TOKEN,

    // Break Tokens
    BAD_TOKEN,
    EOF_TOKEN,

    // Not-parsed Tokens
    COMMENT_TOKEN,
    WHITE_SPACE_TOKEN
}
