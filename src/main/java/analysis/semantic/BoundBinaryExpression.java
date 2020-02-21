package analysis.semantic;

import symbols.NodeType;
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
    public NodeType getType()
    {
        return NodeType.BinaryExpression;
    }

    @Override
    public Class getClassType()
    {
        return this.operator.getResultClassType();
    }
}
