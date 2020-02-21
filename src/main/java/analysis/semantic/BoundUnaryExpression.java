package analysis.semantic;

import identifiers.ExpressionType;
import lombok.Getter;
import identifiers.ObjectType;

public final class BoundUnaryExpression extends BoundExpression
{
    @Getter private final BoundUnaryOperator operator;
    @Getter private final BoundExpression operand;

    public BoundUnaryExpression(BoundUnaryOperator operator, BoundExpression operand)
    {
        this.operator = operator;
        this.operand = operand;
    }

    @Override
    public ExpressionType getExpressionType()
    {
        return ExpressionType.UnaryExpression;
    }

    @Override
    public ObjectType getObjectType()
    {
        return this.operator.getResultObjectType();
    }
}
