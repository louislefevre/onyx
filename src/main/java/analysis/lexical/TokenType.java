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

    // Expression Tokens
    BinaryExpressionToken,
    ParenthesizedExpressionToken,
    LiteralExpressionToken,

    // Break Tokens
    BadToken,
    EOFToken
}
