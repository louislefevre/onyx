package analysis.semantic;

import identifiers.ExpressionType;
import identifiers.ObjectType;
import lombok.Getter;

@Getter
public final class AnnotatedAssignmentExpression extends AnnotatedExpression
{
    private final String name;
    private final AnnotatedExpression expression;
    private final ObjectType objectType;
    private final ExpressionType expressionType;

    public AnnotatedAssignmentExpression(String name, AnnotatedExpression expression)
    {
        this.name = name;
        this.expression = expression;
        this.objectType = expression.getObjectType();
        this.expressionType = ExpressionType.ASSIGNMENT_EXPRESSION;
    }
}
