package analysis.semantic;

import identifiers.ExpressionType;
import identifiers.ObjectType;

public abstract class AnnotatedExpression
{
    public abstract ExpressionType getExpressionType();
    public abstract ObjectType getObjectType();
}
