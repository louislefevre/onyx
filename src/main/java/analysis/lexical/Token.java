package analysis.lexical;

import identifiers.TokenType;
import lombok.Getter;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@Getter
public final class Token
{
    private final TokenType tokenType;
    private final String syntax;
    private final Object value;
    private final int position;
    private final SyntaxSpan span;

    public Token(TokenType tokenType, @NotNull String syntax, Object value, int position)
    {
        this.tokenType = tokenType;
        this.syntax = syntax;
        this.value = value;
        this.position = position;
        this.span = new SyntaxSpan(position, syntax.length());
    }

    public Token(TokenType tokenType, String syntax, int position)
    {
        this(tokenType, syntax, null, position);
    }

    public Token(TokenType tokenType, int position)
    {
        this(tokenType, findSyntax(tokenType), position);
    }

    @Nullable
    @Contract(pure = true)
    private static String findSyntax(@NotNull TokenType tokenType)
    {
        switch (tokenType)
        {
            case PLUS_TOKEN:
                return Syntax.PLUS;
            case MINUS_TOKEN:
                return Syntax.MINUS;
            case STAR_TOKEN:
                return Syntax.STAR;
            case SLASH_TOKEN:
                return Syntax.SLASH;
            case CARET_TOKEN:
                return Syntax.CARET;
            case PERCENT_TOKEN:
                return Syntax.PERCENT;
            case OPEN_PARENTHESIS_TOKEN:
                return Syntax.OPEN_PARENTHESIS;
            case CLOSE_PARENTHESIS_TOKEN:
                return Syntax.CLOSE_PARENTHESIS;
            case AND_TOKEN:
                return Syntax.AND;
            case OR_TOKEN:
                return Syntax.OR;
            case EQUALS_EQUALS_TOKEN:
                return Syntax.EQUALS_EQUALS;
            case EQUALS_TOKEN:
                return Syntax.EQUALS;
            case NOT_EQUALS_TOKEN:
                return Syntax.NOT_EQUALS;
            case NOT_TOKEN:
                return Syntax.NOT;
            case GREATER_TOKEN:
                return Syntax.GREATER;
            case LESS_TOKEN:
                return Syntax.LESS;
            case GREATER_EQUALS_TOKEN:
                return Syntax.GREATER_EQUALS;
            case LESS_EQUALS_TOKEN:
                return Syntax.LESS_EQUALS;
            default:
                return null;
        }
    }
}
