package analysis.semantic;

import lombok.Getter;
import identifiers.ExpressionType;
import identifiers.ObjectType;

public final class BoundBinaryExpression extends BoundExpression
{
    @Getter private final BoundExpression leftTerm;
    @Getter private final BoundBinaryOperator operator;
    @Getter private final BoundExpression rightTerm;

    public BoundBinaryExpression(BoundExpression leftTerm, BoundBinaryOperator operator, BoundExpression rightTerm)
    {
        this.leftTerm = leftTerm;
        this.operator = operator;
        this.rightTerm = rightTerm;
    }

    @Override
    public ExpressionType getExpressionType()
    {
        return ExpressionType.BinaryExpression;
    }

    @Override
    public ObjectType getObjectType()
    {
        return this.operator.getResultObjectType();
    }
}
