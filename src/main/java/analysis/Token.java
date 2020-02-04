package main.java.codeanalysis;

import java.util.List;
import java.util.ArrayList;

public class Token extends Node
{
    private TokenType type;
    private int position;
    private String text;
    private Object value;

    public Token(TokenType type, int position, String text, Object value)
    {
        this.type = type;
        this.position = position;
        this.text = text;
        this.value = value;
    }

    @Override
    public TokenType getType() { return this.type; }
    @Override
    public List<Node> getChildren() { return new ArrayList<>(); } // Collections.emptyList() instead?
    public int getPosition() { return this.position; }
    public String getText() { return this.text; }
    public Object getValue() { return this.value; }
}
