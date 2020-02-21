package analysis.lexical;

import identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

public final class Token extends Node
{
    @Getter private final TokenType tokenType;
    @Getter private final String text;
    @Getter private final Object value;
    @Getter private final int position;
    @Getter private final List<Node> children;

    public Token(TokenType tokenType, String text, Object value, int position)
    {
        this.tokenType = tokenType;
        this.text = text;
        this.value = value;
        this.position = position;
        this.children = new ArrayList<>();
    }

    public Token(TokenType tokenType, String text, int position)
    {
        this(tokenType, text, null, position);
    }

    public Token(TokenType tokenType, int position)
    {
        this(tokenType, null, position);
    }
}
