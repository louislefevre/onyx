package analysis.semantic;

import identifiers.ExpressionType;
import identifiers.ObjectType;
import lombok.Getter;

public final class AnnotatedAssignmentExpression extends AnnotatedExpression
{
    @Getter private final String name;
    @Getter private final AnnotatedExpression expression;
    @Getter private final ObjectType objectType;
    @Getter private final ExpressionType expressionType;

    public AnnotatedAssignmentExpression(String name, AnnotatedExpression expression)
    {
        this.name = name;
        this.expression = expression;
        this.objectType = expression.getObjectType();
        this.expressionType = ExpressionType.ASSIGNMENT_EXPRESSION;
    }
}
