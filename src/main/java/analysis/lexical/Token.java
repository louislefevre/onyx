package analysis.lexical;

import identifiers.TokenType;
import lombok.Getter;
import source.SourceSpan;

@Getter
public final class Token
{
    private final TokenType tokenType;
    private final String syntax;
    private final Object value;
    private final int position;
    private final SourceSpan span;

    public Token(TokenType tokenType, String syntax, Object value, int position)
    {
        this.tokenType = tokenType;
        this.syntax = syntax;
        this.value = value;
        this.position = position;
        this.span = new SourceSpan(position, syntax.length());
    }

    public Token(TokenType tokenType, String syntax, int position)
    {
        this(tokenType, syntax, null, position);
    }
}
