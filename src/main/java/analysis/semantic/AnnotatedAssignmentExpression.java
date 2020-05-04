package analysis.semantic;

import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;
import lombok.Getter;

import static identifiers.AnnotatedExpressionType.ANNOTATED_ASSIGNMENT_EXPRESSION;

@Getter
public final class AnnotatedAssignmentExpression implements AnnotatedExpression
{
    private final String name;
    private final AnnotatedAssignmentOperator operator;
    private final AnnotatedExpression expression;
    private final ObjectType objectType;
    private final AnnotatedExpressionType expressionType;

    public AnnotatedAssignmentExpression(String name, AnnotatedAssignmentOperator operator, AnnotatedExpression expression)
    {
        this.name = name;
        this.operator = operator;
        this.expression = expression;
        this.objectType = expression.getObjectType();
        this.expressionType = ANNOTATED_ASSIGNMENT_EXPRESSION;
    }
}
