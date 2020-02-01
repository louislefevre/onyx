package main.java.codeanalysis;

public abstract class SyntaxNode
{
    public abstract SyntaxKind getKind();

    public abstract Iterable<SyntaxNode> getChildren();
}
