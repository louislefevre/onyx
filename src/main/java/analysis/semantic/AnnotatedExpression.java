package analysis.semantic;

import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;

public interface AnnotatedExpression
{
    AnnotatedExpressionType getExpressionType();

    ObjectType getObjectType();
}
