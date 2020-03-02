package analysis.semantic;

import lombok.Getter;
import identifiers.ExpressionType;
import identifiers.ObjectType;

public final class AnnotatedLiteralExpression extends AnnotatedExpression
{
    @Getter private final Object value;
    @Getter private final ExpressionType expressionType;
    @Getter private final ObjectType objectType;

    public AnnotatedLiteralExpression(Object value)
    {
        this.value = value;
        this.expressionType = ExpressionType.LITERAL_EXPRESSION;
        this.objectType = typeOf(value);
    }

    private static ObjectType typeOf(Object object)
    {
        if(object instanceof Integer)
            return ObjectType.INTEGER_OBJECT;
        else if(object instanceof Boolean)
            return ObjectType.BOOLEAN_OBJECT;
        else
            return ObjectType.NULL_OBJECT;
    }
}
