package analysis.semantic;

import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;
import lombok.Getter;

import static identifiers.AnnotatedExpressionType.ANNOTATED_LITERAL_EXPRESSION;
import static identifiers.ObjectType.*;

@Getter
public final class AnnotatedLiteralExpression implements AnnotatedExpression
{
    private final Object value;
    private final AnnotatedExpressionType expressionType;
    private final ObjectType objectType;

    public AnnotatedLiteralExpression(Object value)
    {
        this.value = value;
        this.expressionType = ANNOTATED_LITERAL_EXPRESSION;
        this.objectType = typeOf(value);
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
