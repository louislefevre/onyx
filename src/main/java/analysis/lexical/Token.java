package analysis.lexical;

import identifiers.TokenType;
import lombok.Getter;

public final class Token
{
    @Getter private final TokenType tokenType;
    @Getter private final String syntax;
    @Getter private final Object value;
    @Getter private final int position;

    public Token(TokenType tokenType, String syntax, Object value, int position)
    {
        this.tokenType = tokenType;
        this.syntax = syntax;
        this.value = value;
        this.position = position;
    }

    public Token(TokenType tokenType, String syntax, int position)
    {
        this(tokenType, syntax, null, position);
    }

    public Token(TokenType tokenType, int position)
    {
        this(tokenType, findSyntax(tokenType), position);
    }

    private static String findSyntax(TokenType tokenType)
    {
        switch(tokenType)
        {
            case PLUS_TOKEN:
                return Syntax.PLUS;
            case MINUS_TOKEN:
                return Syntax.MINUS;
            case STAR_TOKEN:
                return Syntax.STAR;
            case SLASH_TOKEN:
                return Syntax.SLASH;
            case OPEN_PARENTHESIS_TOKEN:
                return Syntax.OPEN_PARENTHESIS;
            case CLOSE_PARENTHESIS_TOKEN:
                return Syntax.CLOSE_PARENTHESIS;
            case AND_TOKEN:
                return Syntax.AND;
            case OR_TOKEN:
                return Syntax.OR;
            case EQUALS_TOKEN:
                return Syntax.EQUALS;
            case NOT_EQUALS_TOKEN:
                return Syntax.NOT_EQUALS;
            case BANG_TOKEN:
                return Syntax.BANG;
            default:
                return null;
        }
    }
}
