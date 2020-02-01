package main.java.codeanalysis;

public class SyntaxToken
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

    public SyntaxKind getKind() { return kind; }
    public int getPosition() { return position; }
    public String getText() { return text; }
    public Object getValue() { return value; }
}
