package analysis.binding;

import analysis.identifiers.BoundNodeType;
import lombok.Getter;

public final class BoundBinaryExpression extends BoundExpression
{
    @Getter private BoundExpression left;
    @Getter private BoundBinaryOperator operator;
    @Getter private BoundExpression right;

    public BoundBinaryExpression(BoundExpression left, BoundBinaryOperator operator, BoundExpression right)
    {
        this.left = left;
        this.operator = operator;
        this.right = right;
    }

    @Override
    public BoundNodeType getKind()
    {
        return BoundNodeType.BinaryExpression;
    }

    @Override
    public Class getType()
    {
        return this.operator.getResultType();
    }
}
