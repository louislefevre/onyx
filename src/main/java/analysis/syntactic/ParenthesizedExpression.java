package main.java.analysis.syntactic;

import main.java.analysis.lexical.Node;
import main.java.analysis.lexical.Token;
import main.java.analysis.lexical.TokenType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class ParenthesizedExpression extends Expression
{
    private final Token openParenthesisToken;
    private final Expression expression;
    private final Token closeParenthesisToken;

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
