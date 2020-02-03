package main.java.codeanalysis;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class NumberExpressionSyntax extends ExpressionSyntax
{
    private SyntaxToken numberToken;

    public NumberExpressionSyntax(SyntaxToken numberToken)
    {
        this.numberToken =  numberToken;
    }

    @Override
    public SyntaxKind getKind() { return SyntaxKind.NumberExpression; }
    @Override
    public List<SyntaxNode> getChildren() { return new ArrayList<>(Arrays.asList(this.numberToken)); }
    public SyntaxToken getNumberToken() { return this.numberToken; }
}
