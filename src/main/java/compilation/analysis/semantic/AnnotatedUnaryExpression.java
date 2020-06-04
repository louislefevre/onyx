package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedExpressionType;
import types.ObjectType;

import static types.AnnotatedExpressionType.ANNOTATED_UNARY_EXPRESSION;

@Getter
public final class AnnotatedUnaryExpression implements AnnotatedExpression
{
    private final AnnotatedUnaryOperator operator;
    private final AnnotatedExpression operand;
    private final AnnotatedExpressionType expressionType;
    private final ObjectType objectType;

    public AnnotatedUnaryExpression(AnnotatedUnaryOperator operator, AnnotatedExpression operand)
    {
        this.operator = operator;
        this.operand = operand;
        this.expressionType = ANNOTATED_UNARY_EXPRESSION;
        this.objectType = operator.getResultObjectType();
    }
}
