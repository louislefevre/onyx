package analysis.lexical;

public enum TokenType
{
    NumberToken,
    WhiteSpaceToken,

    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,

    OpenParenthesisToken,
    CloseParenthesisToken,

    BinaryExpressionToken,
    ParenthesizedExpressionToken,
    NumberExpressionToken,

    BadToken,
    EOFToken
}
