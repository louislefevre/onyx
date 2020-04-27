package analysis.semantic;

import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;

public abstract class AnnotatedExpression
{
    public abstract AnnotatedExpressionType getAnnotatedExpressionType();

    public abstract ObjectType getObjectType();
}
