package analysis.semantic;

import symbols.NodeType;
import lombok.Getter;
import symbols.ObjectType;
import symbols.Syntax;

public final class BoundLiteralExpression extends BoundExpression
{
    @Getter private final Object value;

    public BoundLiteralExpression(Object value)
    {
        this.value = value;
    }

    @Override
    public NodeType getNodeType()
    {
        return NodeType.LiteralExpression;
    }

    @Override
    public ObjectType getObjectType()
    {
        return Syntax.getObjectType(this.value);
    }
}
