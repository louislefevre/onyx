package analysis.binding;

import lombok.Getter;

public final class BoundBinaryExpression extends BoundExpression
{
    @Getter
    private BoundExpression left;
    @Getter private BoundBinaryOperatorKind operatorKind;
    @Getter private BoundExpression right;

    public BoundBinaryExpression(BoundExpression left, BoundBinaryOperatorKind operatorKind, BoundExpression right)
    {
        this.left = left;
        this.operatorKind = operatorKind;
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
        return this.left.getType();
    }
}
