package analysis.semantic;

import lombok.Getter;
import identifiers.ExpressionType;
import identifiers.ObjectType;

public final class BoundLiteralExpression extends BoundExpression
{
    @Getter private final Object value;

    public BoundLiteralExpression(Object value)
    {
        this.value = value;
    }

    @Override
    public ExpressionType getExpressionType()
    {
        return ExpressionType.LiteralExpression;
    }

    @Override
    public ObjectType getObjectType()
    {
        return typeOf(this.value);
    }

    private static ObjectType typeOf(Object object)
    {
        if(object instanceof Integer)
            return ObjectType.IntegerObject;
        else if(object instanceof Boolean)
            return ObjectType.BooleanObject;
        else
            return ObjectType.NullObject;
    }
}
