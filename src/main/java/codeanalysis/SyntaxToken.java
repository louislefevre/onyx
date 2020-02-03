package main.java.codeanalysis;

import java.util.List;
import java.util.ArrayList;

public class SyntaxToken extends SyntaxNode
{
    private SyntaxKind kind;
    private int position;
    private String text;
    private Object value;

    public SyntaxToken(SyntaxKind kind, int position, String text, Object value)
    {
        this.kind = kind;
        this.position = position;
        this.text = text;
        this.value = value;
    }

    @Override
    public SyntaxKind getKind() { return kind; }
    @Override
    public List<SyntaxNode> getChildren() { return new ArrayList<>(); } // Collections.emptyList() instead?
    public int getPosition() { return position; }
    public String getText() { return text; }
    public Object getValue() { return value; }
}
