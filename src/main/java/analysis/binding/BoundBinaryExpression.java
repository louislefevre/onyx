package analysis.binding;

import analysis.identifiers.BoundNodeType;
import lombok.Getter;

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
    public BoundNodeType getType()
    {
        return BoundNodeType.BinaryExpression;
    }

    @Override
    public Class getClassType()
    {
        return this.operator.getResultClassType();
    }
}
