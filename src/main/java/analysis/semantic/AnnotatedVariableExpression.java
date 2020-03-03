package analysis.semantic;

import identifiers.ExpressionType;
import identifiers.ObjectType;
import lombok.Getter;

public final class AnnotatedVariableExpression extends AnnotatedExpression
{
    @Getter private final String name;
    @Getter private final ObjectType objectType;
    @Getter private final ExpressionType expressionType;

    public AnnotatedVariableExpression(String name, ObjectType type)
    {
        this.name = name;
        this.objectType = type;
        this.expressionType = ExpressionType.VARIABLE_EXPRESSION;
    }
}
