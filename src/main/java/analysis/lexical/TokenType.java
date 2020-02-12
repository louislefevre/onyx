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
    LiteralExpressionToken,
    UnaryExpressionToken,
    BinaryExpressionToken,
    ParenthesizedExpressionToken,

    // Break Tokens
    BadToken,
    EOFToken,
}
