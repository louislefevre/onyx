package analysis.lexical;

import identifiers.TokenType;
import lombok.Getter;
import source.SourceSpan;

@Getter
public final class Token
{
    private final TokenType type;
    private final String syntax;
    private final Object value;
    private final int position;
    private final SourceSpan span;

    public Token(TokenType type, String syntax, Object value, int position)
    {
        this.type = type;
        this.syntax = syntax;
        this.value = value;
        this.position = position;
        this.span = new SourceSpan(position, syntax.length());
    }

    public Token(TokenType type, String syntax, int position)
    {
        this(type, syntax, null, position);
    }
}
