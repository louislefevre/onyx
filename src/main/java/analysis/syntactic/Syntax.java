package analysis.syntactic;

import analysis.lexical.TokenType;

public final class Syntax
{
    private Syntax() {}

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
}
