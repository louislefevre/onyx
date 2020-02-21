package analysis.semantic;

import symbols.ExpressionType;
import symbols.ObjectType;

public abstract class BoundExpression
{
    public abstract ExpressionType getExpressionType();
    public abstract ObjectType getObjectType();
}
