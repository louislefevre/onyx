package analysis.semantic;

import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;
import lombok.Getter;

@Getter
public final class AnnotatedVariableExpression extends AnnotatedExpression
{
    private final String name;
    private final ObjectType objectType;
    private final AnnotatedExpressionType annotatedExpressionType;

    public AnnotatedVariableExpression(String name, ObjectType type)
    {
        this.name = name;
        this.objectType = type;
        this.annotatedExpressionType = AnnotatedExpressionType.ANNOTATED_VARIABLE_EXPRESSION;
    }
}
