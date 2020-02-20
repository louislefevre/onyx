package analysis.binding;

import analysis.identifiers.BoundNodeType;
import lombok.Getter;

public final class BoundLiteralExpression extends BoundExpression
{
    @Getter
    private Object value;

    public BoundLiteralExpression(Object value)
    {
        this.value = value;
    }

    @Override
    public BoundNodeType getKind() {
        return BoundNodeType.LiteralExpression;
    }

    @Override
    public Class getType() {
        return this.value.getClass();
    }
}
