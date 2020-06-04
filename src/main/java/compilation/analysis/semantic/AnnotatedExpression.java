package compilation.analysis.semantic;

import types.AnnotatedExpressionType;
import types.ObjectType;

public interface AnnotatedExpression
{
    AnnotatedExpressionType getExpressionType();

    ObjectType getObjectType();
}
