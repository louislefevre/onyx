package compilation.analysis.lexical;

import lombok.Getter;
import source.SourceSpan;
import types.TokenType;

/**
 * The Token class is used to store information about characters in the form of a Token.
 * <p>
 * It takes the type, syntax, value, and position of a piece of text, and is then used to represent
 * that text from that point on.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class Token
{
    private final TokenType type;
    private final String syntax;
    private final Object value;
    private final int position;
    private final SourceSpan span;

    /**
     * Constructs a Token object initialised with information about a piece of text.
     * <p>
     * A SourceSpan object is automatically generated based on the position and length of the syntax.
     *
     * @param type The TokenType of the text
     * @param syntax The text itself
     * @param value The value of the text
     * @param position The position of the text relative to the String it originates from
     */
    public Token(TokenType type, String syntax, Object value, int position)
    {
        this.type = type;
        this.syntax = syntax;
        this.value = value;
        this.position = position;
        this.span = new SourceSpan(position, syntax.length());
    }

    /**
     * Constructs a Token object initialised with information about a piece of text, but without a value.
     * <p>
     * A SourceSpan object is automatically generated based on the position and length of the syntax.
     * The Tokens 'value' field is set to null.
     *
     * @param type The TokenType of the text
     * @param syntax The text itself
     * @param position The position of the text relative to the String it originates from
     */
    public Token(TokenType type, String syntax, int position)
    {
        this(type, syntax, null, position);
    }
}
