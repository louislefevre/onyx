package analysis.binding;

import lombok.Getter;

final class BoundUnaryExpression extends BoundExpression
{
    @Getter
    private BoundUnaryOperatorKind operatorKind;
    @Getter private BoundExpression operand;

    public BoundUnaryExpression(BoundUnaryOperatorKind operatorKind, BoundExpression operand)
    {
        this.operatorKind = operatorKind;
        this.operand = operand;
    }

    @Override
    public BoundNodeKind getKind() {
        return BoundNodeKind.UnaryExpression;
    }

    @Override
    public Class getType() {
        return this.operand.getType();
    }
}
