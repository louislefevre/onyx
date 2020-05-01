package analysis.syntax;

import identifiers.TokenType;

public final class OperatorPrecedence
{
    public static int getUnaryOperatorPrecedence(TokenType tokenType)
    {
        switch (tokenType)
        {
            case PLUS_TOKEN:
            case MINUS_TOKEN:
            case NOT_TOKEN:
                return 6;
            default:
                return 0;
        }
    }

    public static int getBinaryOperatorPrecedence(TokenType tokenType)
    {
        switch (tokenType)
        {
            case CARET_TOKEN:
                return 6;
            case STAR_TOKEN:
            case SLASH_TOKEN:
            case PERCENT_TOKEN:
                return 5;
            case PLUS_TOKEN:
            case MINUS_TOKEN:
                return 4;
            case EQUALS_EQUALS_TOKEN:
            case NOT_EQUALS_TOKEN:
            case GREATER_TOKEN:
            case LESS_TOKEN:
            case GREATER_EQUALS_TOKEN:
            case LESS_EQUALS_TOKEN:
                return 3;
            case AND_TOKEN:
                return 2;
            case OR_TOKEN:
                return 1;
            default:
                return 0;
        }
    }
}
