package main.java.codeanalysis;

import java.util.Collections;

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
    public Iterable<SyntaxNode> getChildren() { return Collections.emptyList(); }
    public int getPosition() { return position; }
    public String getText() { return text; }
    public Object getValue() { return value; }
}
