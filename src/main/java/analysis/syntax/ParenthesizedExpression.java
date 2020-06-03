package analysis.syntax;

import analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.ExpressionType;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static types.ExpressionType.PARENTHESIZED_EXPRESSION;

@Getter
public final class ParenthesizedExpression implements Expression
{
    private final Token openParenthesisToken;
    private final Expression expression;
    private final Token closeParenthesisToken;
    private final ExpressionType expressionType;
    private final SourceSpan span;
    private final Queue<Object> children;

    public ParenthesizedExpression(Token openParenthesisToken, Expression expression, Token closeParenthesisToken)
    {
        this.openParenthesisToken = openParenthesisToken;
        this.expression = expression;
        this.closeParenthesisToken = closeParenthesisToken;
        this.expressionType = PARENTHESIZED_EXPRESSION;
        this.span = SourceSpan.inRange(openParenthesisToken.getSpan().getStart(), closeParenthesisToken.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(openParenthesisToken, expression, closeParenthesisToken));
    }
}
