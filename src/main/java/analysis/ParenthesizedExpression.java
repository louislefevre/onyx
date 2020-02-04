package main.java.analysis;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class ParenthesizedExpression extends Expression
{
    private Token openParenthesisToken;
    private Expression expression;
    private Token closeParenthesisToken;

    public ParenthesizedExpression(Token openParenthesisToken, Expression expression, Token closeParenthesisToken)
    {
        this.openParenthesisToken = openParenthesisToken;
        this.expression = expression;
        this.closeParenthesisToken = closeParenthesisToken;
    }

    @Override
    public TokenType getType()
    {
        return TokenType.ParenthesizedExpressionToken;
    }

    @Override
    public List<Node> getChildren()
    {
        return new ArrayList<>(Arrays.asList(this.openParenthesisToken, this.expression, this.closeParenthesisToken));
    }

    public Expression getExpression()
    {
        return this.expression;
    }
}
