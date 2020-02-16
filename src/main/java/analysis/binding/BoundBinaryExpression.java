package analysis.binding;

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
    public BoundNodeKind getKind()
    {
        return BoundNodeKind.BinaryExpression;
    }

    @Override
    public Class getType()
    {
        return this.operator.getResultType();
    }
}
