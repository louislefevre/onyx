package analysis.semantic;

import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;

public interface AnnotatedExpression
{
    AnnotatedExpressionType getAnnotatedExpressionType();

    ObjectType getObjectType();
}
