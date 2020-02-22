package analysis.lexical;

import identifiers.TokenType;
import lombok.Getter;
import symbols.Symbols;

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
                return Symbols.PLUS;
            case MINUS_TOKEN:
                return Symbols.MINUS;
            case STAR_TOKEN:
                return Symbols.STAR;
            case SLASH_TOKEN:
                return Symbols.SLASH;
            case OPEN_PARENTHESIS_TOKEN:
                return Symbols.OPEN_PARENTHESIS;
            case CLOSE_PARENTHESIS_TOKEN:
                return Symbols.CLOSE_PARENTHESIS;
            case AND_TOKEN:
                return Symbols.AND;
            case OR_TOKEN:
                return Symbols.OR;
            case EQUALS_TOKEN:
                return Symbols.EQUALS;
            case NOT_EQUALS_TOKEN:
                return Symbols.NOT_EQUALS;
            case BANG_TOKEN:
                return Symbols.BANG;
            default:
                return null;
        }
    }
}
