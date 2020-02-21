package analysis.semantic;

import symbols.ObjectType;

public abstract class BoundExpression extends BoundNode
{
    public abstract ObjectType getObjectType();
}
