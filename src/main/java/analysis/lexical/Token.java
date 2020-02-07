package analysis.lexical;

import java.util.List;
import java.util.ArrayList;

public class Token extends Node
{
    private final TokenType type;
    private final String text;
    private final Object value;
    private final int position;

    public Token(TokenType type, String text, Object value, int position)
    {
        this.type = type;
        this.text = text;
        this.value = value;
        this.position = position;
    }

    @Override
    public TokenType getType()
    {
        return this.type;
    }

    @Override
    public List<Node> getChildren()
    {
        return new ArrayList<>(); // Collections.emptyList() instead?
    }

    public Object getValue()
    {
        return this.value;
    }

    public int getPosition()
    {
        return this.position;
    }
}
