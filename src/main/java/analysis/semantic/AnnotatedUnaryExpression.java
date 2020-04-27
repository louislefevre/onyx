package analysis.semantic;

import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;
import lombok.Getter;

@Getter
public final class AnnotatedUnaryExpression extends AnnotatedExpression
{
    private final AnnotatedUnaryOperator operator;
    private final AnnotatedExpression operand;
    private final AnnotatedExpressionType annotatedExpressionType;
    private final ObjectType objectType;

    public AnnotatedUnaryExpression(AnnotatedUnaryOperator operator, AnnotatedExpression operand)
    {
        this.operator = operator;
        this.operand = operand;
        this.annotatedExpressionType = AnnotatedExpressionType.ANNOTATED_UNARY_EXPRESSION;
        this.objectType = operator.getResultObjectType();
    }
}
