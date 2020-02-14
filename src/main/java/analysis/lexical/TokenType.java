package analysis.lexical;

public enum TokenType
{
    // Primary Tokens
    NumberToken,
    WhiteSpaceToken,

    // Operator Tokens
    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,
    OpenParenthesisToken,
    CloseParenthesisToken,
    BangToken,
    AndToken,
    OrToken,

    // Keywords
    FalseKeyword,
    TrueKeyword,
    IdentifierKeyword,

    // Expression Tokens
    LiteralExpression,
    UnaryExpression,
    BinaryExpression,
    ParenthesizedExpression,

    // Break Tokens
    BadToken,
    EOFToken,
}
