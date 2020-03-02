package analysis.semantic;

import identifiers.ExpressionType;
import lombok.Getter;
import identifiers.ObjectType;

public final class AnnotatedUnaryExpression extends AnnotatedExpression
{
    @Getter private final AnnotatedUnaryOperator operator;
    @Getter private final AnnotatedExpression operand;
    @Getter private final ExpressionType expressionType;
    @Getter private final ObjectType objectType;

    public AnnotatedUnaryExpression(AnnotatedUnaryOperator operator, AnnotatedExpression operand)
    {
        this.operator = operator;
        this.operand = operand;
        this.expressionType = ExpressionType.UNARY_EXPRESSION;
        this.objectType = operator.getResultObjectType();
    }
}
