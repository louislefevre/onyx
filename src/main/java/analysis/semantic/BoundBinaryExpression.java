package analysis.semantic;

import lombok.Getter;
import symbols.NodeType;
import symbols.ObjectType;

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
    public NodeType getNodeType()
    {
        return NodeType.BinaryExpression;
    }

    @Override
    public ObjectType getObjectType()
    {
        return this.operator.getResultObjectType();
    }
}
