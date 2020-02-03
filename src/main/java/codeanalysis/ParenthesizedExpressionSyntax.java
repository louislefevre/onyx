package main.java.codeanalysis;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class ParenthesizedExpressionSyntax extends ExpressionSyntax
{
    private SyntaxToken openParenthesisToken;
    private ExpressionSyntax expression;
    private SyntaxToken closeParenthesisToken;

    public ParenthesizedExpressionSyntax(SyntaxToken openParenthesisToken, ExpressionSyntax expression, SyntaxToken closeParenthesisToken)
    {
        this.openParenthesisToken = openParenthesisToken;
        this.expression = expression;
        this.closeParenthesisToken = closeParenthesisToken;
    }

    @Override
    public SyntaxKind getKind() { return SyntaxKind.ParenthesizedExpression; }
    @Override
    public List<SyntaxNode> getChildren() { return new ArrayList<>(Arrays.asList(this.openParenthesisToken, this.expression, this.closeParenthesisToken)); }
    public SyntaxToken getOpenParenthesisToken() { return this.openParenthesisToken; }
    public ExpressionSyntax getExpression() { return this.expression; }
    public SyntaxToken getClosedParenthesisToken() { return this.closeParenthesisToken; }
}
