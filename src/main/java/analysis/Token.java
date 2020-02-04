package main.java.analysis;

import java.util.List;
import java.util.ArrayList;

public class Token extends Node
{
    private TokenType type;
    private String text;
    private Object value;
    private int position;

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

    public String getText()
    {
        return this.text;
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
