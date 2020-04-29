package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Getter
public final class ParenthesizedExpression implements Expression
{
    private final Expression expression;
    private final ExpressionType expressionType;
    private final List<Object> children;

    public ParenthesizedExpression(Token openParenthesisToken, Expression expression, Token closeParenthesisToken)
    {
        this.expression = expression;
        this.expressionType = ExpressionType.PARENTHESIZED_EXPRESSION;
        this.children = new ArrayList<>(Arrays.asList(openParenthesisToken, expression, closeParenthesisToken));
    }
}
