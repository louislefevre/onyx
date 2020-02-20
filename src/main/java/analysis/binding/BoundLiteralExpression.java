package analysis.binding;

import analysis.identifiers.BoundNodeType;
import lombok.Getter;

public final class BoundLiteralExpression extends BoundExpression
{
    @Getter private final Object value;

    public BoundLiteralExpression(Object value)
    {
        this.value = value;
    }

    @Override
    public BoundNodeType getType()
    {
        return BoundNodeType.LiteralExpression;
    }

    @Override
    public Class getClassType()
    {
        return this.value.getClass();
    }
}
