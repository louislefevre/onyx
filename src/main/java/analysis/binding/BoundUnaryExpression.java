package analysis.binding;

import analysis.identifiers.BoundNodeType;
import lombok.Getter;

public final class BoundUnaryExpression extends BoundExpression
{
    @Getter private BoundUnaryOperator operator;
    @Getter private BoundExpression operand;

    public BoundUnaryExpression(BoundUnaryOperator operator, BoundExpression operand)
    {
        this.operator = operator;
        this.operand = operand;
    }

    @Override
    public BoundNodeType getKind() {
        return BoundNodeType.UnaryExpression;
    }

    @Override
    public Class getType() {
        return this.operator.getResultType();
    }
}
