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
        this(tokenType, syntax, findValue(tokenType), position);
    }

    public Token(TokenType tokenType, int position)
    {
        this(tokenType, findSyntax(tokenType), position);
    }

    @Contract(pure = true)
    private static @Nullable Object findValue(@NotNull TokenType tokenType)
    {
        switch (tokenType)
        {
            case TRUE_KEYWORD_TOKEN:
                return true;
            case FALSE_KEYWORD_TOKEN:
                return false;
            default:
                return null;
        }
    }

    @Nullable
    @Contract(pure = true)
    private static String findSyntax(@NotNull TokenType tokenType)
    {
        switch (tokenType)
        {
            case PLUS_TOKEN:
                return Syntax.PLUS.getSyntax();
            case MINUS_TOKEN:
                return Syntax.MINUS.getSyntax();
            case STAR_TOKEN:
                return Syntax.STAR.getSyntax();
            case SLASH_TOKEN:
                return Syntax.SLASH.getSyntax();
            case CARET_TOKEN:
                return Syntax.CARET.getSyntax();
            case PERCENT_TOKEN:
                return Syntax.PERCENT.getSyntax();
            case OPEN_PARENTHESIS_TOKEN:
                return Syntax.OPEN_PARENTHESIS.getSyntax();
            case CLOSE_PARENTHESIS_TOKEN:
                return Syntax.CLOSE_PARENTHESIS.getSyntax();
            case EQUALS_EQUALS_TOKEN:
                return Syntax.EQUALS_EQUALS.getSyntax();
            case EQUALS_TOKEN:
                return Syntax.EQUALS.getSyntax();
            case NOT_EQUALS_TOKEN:
                return Syntax.NOT_EQUALS.getSyntax();
            case NOT_TOKEN:
                return Syntax.NOT.getSyntax();
            case GREATER_TOKEN:
                return Syntax.GREATER.getSyntax();
            case LESS_TOKEN:
                return Syntax.LESS.getSyntax();
            case GREATER_EQUALS_TOKEN:
                return Syntax.GREATER_EQUALS.getSyntax();
            case LESS_EQUALS_TOKEN:
                return Syntax.LESS_EQUALS.getSyntax();
            case AND_TOKEN:
                return Syntax.AND.getSyntax();
            case OR_TOKEN:
                return Syntax.OR.getSyntax();
            case TRUE_KEYWORD_TOKEN:
                return Syntax.TRUE.getSyntax();
            case FALSE_KEYWORD_TOKEN:
                return Syntax.FALSE.getSyntax();
            case EOF_TOKEN:
                return Syntax.EOF.getSyntax();
            default:
                return null;
        }
    }
}
