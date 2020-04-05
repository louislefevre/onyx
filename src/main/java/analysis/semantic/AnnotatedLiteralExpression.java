package analysis.semantic;

import lombok.Getter;
import identifiers.ExpressionType;
import identifiers.ObjectType;
import util.Utilities;

@Getter
public final class AnnotatedLiteralExpression extends AnnotatedExpression
{
    private final Object value;
    private final ExpressionType expressionType;
    private final ObjectType objectType;

    public AnnotatedLiteralExpression(Object value)
    {
        this.value = value;
        this.expressionType = ExpressionType.LITERAL_EXPRESSION;
        this.objectType = Utilities.typeOf(value);
    }
}
