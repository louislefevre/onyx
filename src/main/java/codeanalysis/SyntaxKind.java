package main.java.codeanalysis;

public enum SyntaxKind
{
    NumberToken,
    WhiteSpace,

    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,

    OpenParenthesisToken,
    CloseParenthesisToken,

    BinaryExpression,
    ParenthesizedExpression,
    NumberExpression,

    BadToken,
    EndOfFileToken
}
