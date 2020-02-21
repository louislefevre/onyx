package identifiers;

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
    FalseKeywordToken,
    TrueKeywordToken,
    IdentifierKeywordToken,

    // Expression Tokens
    LiteralExpressionToken,
    UnaryExpressionToken,
    BinaryExpressionToken,
    ParenthesizedExpressionToken,

    // Break Tokens
    BadToken,
    EOFToken
}
