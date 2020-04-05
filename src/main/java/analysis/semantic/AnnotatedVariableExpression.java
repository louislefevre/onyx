package analysis.semantic;

import identifiers.ExpressionType;
import identifiers.ObjectType;
import lombok.Getter;

@Getter
public final class AnnotatedVariableExpression extends AnnotatedExpression
{
    private final String name;
    private final ObjectType objectType;
    private final ExpressionType expressionType;

    public AnnotatedVariableExpression(String name, ObjectType type)
    {
        this.name = name;
        this.objectType = type;
        this.expressionType = ExpressionType.VARIABLE_EXPRESSION;
    }
}
