package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

@Getter
public final class ParenthesizedExpression implements Expression
{
    private final Token openParenthesisToken;
    private final Expression expression;
    private final Token closeParenthesisToken;
    private final ExpressionType expressionType;
    private final Queue<Object> children;

    public ParenthesizedExpression(Token openParenthesisToken, Expression expression, Token closeParenthesisToken)
    {
        this.openParenthesisToken = openParenthesisToken;
        this.expression = expression;
        this.closeParenthesisToken = closeParenthesisToken;
        this.expressionType = ExpressionType.PARENTHESIZED_EXPRESSION;
        this.children = new LinkedList<>(Arrays.asList(openParenthesisToken, expression, closeParenthesisToken));
    }
}
