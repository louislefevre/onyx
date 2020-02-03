package main.java.codeanalysis;

import java.util.List;

public abstract class SyntaxNode
{
    public abstract SyntaxKind getKind();

    public abstract List<SyntaxNode> getChildren();
}
