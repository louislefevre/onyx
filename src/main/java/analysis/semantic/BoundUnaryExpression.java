package analysis.semantic;

import identifiers.ExpressionType;
import lombok.Getter;
import identifiers.ObjectType;

public final class BoundUnaryExpression extends BoundExpression
{
    @Getter private final BoundUnaryOperator operator;
    @Getter private final BoundExpression operand;
    @Getter private final ExpressionType expressionType;
    @Getter private final ObjectType objectType;

    public BoundUnaryExpression(BoundUnaryOperator operator, BoundExpression operand)
    {
        this.operator = operator;
        this.operand = operand;
        this.expressionType = ExpressionType.UNARY_EXPRESSION;
        this.objectType = this.operator.getResultObjectType();
    }
}
