package compilation.analysis.semantic;

import types.AnnotatedExpressionType;
import types.ObjectType;

/**
 * The AnnotatedExpression interface is used to represent annotated expressions declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public interface AnnotatedExpression
{
    /**
     * Return the ObjectType of an AnnotatedExpression object
     *
     * @return the ObjectType of the AnnotatedExpression
     */
    ObjectType getObjectType();

    /**
     * Return the AnnotatedExpressionType of an AnnotatedExpression object
     *
     * @return the type of the AnnotatedExpression
     */
    AnnotatedExpressionType getExpressionType();
}
