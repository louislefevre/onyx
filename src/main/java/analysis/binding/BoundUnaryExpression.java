package analysis.binding;

import analysis.identifiers.NodeType;
import lombok.Getter;

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
    public NodeType getType()
    {
        return NodeType.UnaryExpression;
    }

    @Override
    public Class getClassType()
    {
        return this.operator.getResultClassType();
    }
}
