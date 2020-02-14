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
                return 3;
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
                return 2;
            case PlusToken:
            case MinusToken:
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
                return TokenType.TrueKeywordToken;
            case "false":
                return TokenType.FalseKeywordToken;
            default:
                return TokenType.IdentifierToken;
        }
    }
}
