package symbols;

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
    EqualsToken,
    NotEqualsToken,

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
    EOFToken
}
