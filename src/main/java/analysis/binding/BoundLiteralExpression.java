package analysis.binding;

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
    public BoundNodeKind getKind() {
        return BoundNodeKind.LiteralExpression;
    }

    @Override
    public Class getType() {
        return this.value.getClass();
    }
}
