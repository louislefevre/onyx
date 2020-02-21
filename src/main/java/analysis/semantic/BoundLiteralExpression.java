package analysis.semantic;

import symbols.NodeType;
import lombok.Getter;

public final class BoundLiteralExpression extends BoundExpression
{
    @Getter private final Object value;

    public BoundLiteralExpression(Object value)
    {
        this.value = value;
    }

    @Override
    public NodeType getType()
    {
        return NodeType.LiteralExpression;
    }

    @Override
    public Class getClassType()
    {
        return this.value.getClass();
    }
}
