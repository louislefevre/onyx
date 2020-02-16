package analysis.syntactic;

import analysis.lexical.TokenType;

public final class Syntax
{
    private Syntax() {}

    public static int getUnaryOperatorPrecedence(TokenType type)
    {
        switch(type)
        {
            case PlusToken:
            case MinusToken:
            case BangToken:
                return 6;
            default:
                return 0;
        }
    }

    public static int getBinaryOperatorPrecedence(TokenType type)
    {
        switch(type)
        {
            case StarToken:
            case SlashToken:
                return 5;
            case PlusToken:
            case MinusToken:
                return 4;
            case EqualsToken:
            case NotEqualsToken:
                return 3;
            case AndToken:
                return 2;
            case OrToken:
                return 1;
            default:
                return 0;
        }
    }

    public static TokenType getKeywordKind(String text)
    {
        switch(text)
        {
            case "true":
                return TokenType.TrueKeyword;
            case "false":
                return TokenType.FalseKeyword;
            default:
                return TokenType.IdentifierKeyword;
        }
    }
}
