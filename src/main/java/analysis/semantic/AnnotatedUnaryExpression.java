package analysis.semantic;

import identifiers.ExpressionType;
import lombok.Getter;
import identifiers.ObjectType;
import org.jetbrains.annotations.NotNull;

@Getter
public final class AnnotatedUnaryExpression extends AnnotatedExpression
{
    private final AnnotatedUnaryOperator operator;
    private final AnnotatedExpression operand;
    private final ExpressionType expressionType;
    private final ObjectType objectType;

    public AnnotatedUnaryExpression(@NotNull AnnotatedUnaryOperator operator, AnnotatedExpression operand)
    {
        this.operator = operator;
        this.operand = operand;
        this.expressionType = ExpressionType.UNARY_EXPRESSION;
        this.objectType = operator.getResultObjectType();
    }
}
