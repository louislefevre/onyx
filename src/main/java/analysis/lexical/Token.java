package analysis.lexical;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

public final class Token extends Node
{
    @Getter private final TokenType type;
    @Getter private final String text;
    @Getter private final Object value;
    @Getter private final int position;
    @Getter private final List<Node> children;

    public Token(TokenType type, String text, Object value, int position)
    {
        this.type = type;
        this.text = text;
        this.value = value;
        this.position = position;
        this.children = new ArrayList<>();
    }

    public Token(TokenType type, String text, int position)
    {
        this(type, text, null, position);
    }

    public Token(TokenType type, int position)
    {
        this(type, null, position);
    }
}
