package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;
import source.SourceSpan;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static identifiers.ExpressionType.UNARY_EXPRESSION;

@Getter
public final class UnaryExpression implements Expression
{
    private final Token operatorToken;
    private final Expression operand;
    private final ExpressionType expressionType;
    private final SourceSpan span;
    private final Queue<Object> children;

    public UnaryExpression(Token operatorToken, Expression operand)
    {
        this.operatorToken = operatorToken;
        this.operand = operand;
        this.expressionType = UNARY_EXPRESSION;
        this.span = SourceSpan.inRange(operatorToken.getSpan().getStart(), operand.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(operatorToken, operand));
    }
}
