package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedExpressionType;
import types.ObjectType;

import static types.AnnotatedExpressionType.ANNOTATED_LITERAL_EXPRESSION;
import static types.ObjectType.*;

/**
 * The AnnotatedLiteralExpression class is used to store information about annotated literal expressions declared
 * during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedLiteralExpression implements AnnotatedExpression
{
    private final Object value;
    private final ObjectType objectType;
    private final AnnotatedExpressionType expressionType;

    /**
     * Constructs an AnnotatedLiteralExpression object, initialised with the expressions contents.
     * <p>
     * The ObjectType is automatically added based on the type of the passed value.
     *
     * @param value The value of the literal
     */
    public AnnotatedLiteralExpression(Object value)
    {
        this.value = value;
        this.objectType = typeOf(value);
        this.expressionType = ANNOTATED_LITERAL_EXPRESSION;
    }

    private static ObjectType typeOf(Object object)
    {
        if (object instanceof Integer)
            return INTEGER_OBJECT;
        else if (object instanceof Double)
            return DOUBLE_OBJECT;
        else if (object instanceof String)
            return STRING_OBJECT;
        else if (object instanceof Boolean)
            return BOOLEAN_OBJECT;
        else
            return NULL_OBJECT;
    }
}
