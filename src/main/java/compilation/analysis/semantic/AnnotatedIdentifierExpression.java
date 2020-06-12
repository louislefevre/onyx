package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedExpressionType;
import types.ObjectType;

import static types.AnnotatedExpressionType.ANNOTATED_IDENTIFIER_EXPRESSION;

/**
 * The AnnotatedIdentifierExpression class is used to store information about annotated identifier expressions declared
 * during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedIdentifierExpression implements AnnotatedExpression
{
    private final String name;
    private final ObjectType objectType;
    private final AnnotatedExpressionType expressionType;

    /**
     * Constructs an AnnotatedIdentifierExpression object, initialised with the expressions contents.
     *
     * @param name The name of the identifier
     * @param objectType The data type of the identifier
     */
    public AnnotatedIdentifierExpression(String name, ObjectType objectType)
    {
        this.name = name;
        this.objectType = objectType;
        this.expressionType = ANNOTATED_IDENTIFIER_EXPRESSION;
    }
}
