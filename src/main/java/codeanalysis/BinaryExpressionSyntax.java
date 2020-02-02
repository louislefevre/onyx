package main.java.codeanalysis;

import java.util.ArrayList;
import java.util.Arrays;

public final class BinaryExpressionSyntax extends ExpressionSyntax
{
    private ExpressionSyntax left;
    private SyntaxToken operatorToken;
    private ExpressionSyntax right;

    public BinaryExpressionSyntax(ExpressionSyntax left, SyntaxToken operatorToken, ExpressionSyntax right)
    {
        this.left = left;
        this.operatorToken = operatorToken;
        this.right = right;
    }

    @Override
    public SyntaxKind getKind() { return SyntaxKind.BinaryExpression; }
    @Override
    public Iterable<SyntaxNode> getChildren() { return new ArrayList<>(Arrays.asList(this.left, this.operatorToken, this.right)); }
    public ExpressionSyntax getLeft() { return this.left; }
    public SyntaxToken getOperatorToken() { return this.operatorToken; }
    public ExpressionSyntax getRight() { return this.right; }
}
