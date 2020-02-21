package analysis.lexical;

import identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

public final class Token extends Node
{
    @Getter private final TokenType tokenType;
    @Getter private final String syntax;
    @Getter private final Object value;
    @Getter private final int position;
    @Getter private final List<Node> children;

    public Token(TokenType tokenType, String syntax, Object value, int position)
    {
        this.tokenType = tokenType;
        this.syntax = syntax;
        this.value = value;
        this.position = position;
        this.children = new ArrayList<>();
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
                return "+";
            case MINUS_TOKEN:
                return "-";
            case STAR_TOKEN:
                return "*";
            case SLASH_TOKEN:
                return "/";
            case OPEN_PARENTHESIS_TOKEN:
                return "(";
            case CLOSE_PARENTHESIS_TOKEN:
                return ")";
            case AND_TOKEN:
                return "&&";
            case OR_TOKEN:
                return "||";
            case EQUALS_TOKEN:
                return "==";
            case NOT_EQUALS_TOKEN:
                return "!=";
            case BANG_TOKEN:
                return "!";
            default:
                return null;
        }
    }
}
