package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedExpressionType;
import types.ObjectType;

import static types.AnnotatedExpressionType.ANNOTATED_ASSIGNMENT_EXPRESSION;

@Getter
public final class AnnotatedAssignmentExpression implements AnnotatedExpression
{
    private final AnnotatedIdentifierExpression identifier;
    private final AnnotatedAssignmentOperator operator;
    private final AnnotatedExpression expression;
    private final ObjectType objectType;
    private final AnnotatedExpressionType expressionType;

    public AnnotatedAssignmentExpression(AnnotatedIdentifierExpression identifier, AnnotatedAssignmentOperator operator, AnnotatedExpression expression)
    {
        this.identifier = identifier;
        this.operator = operator;
        this.expression = expression;
        this.objectType = expression.getObjectType();
        this.expressionType = ANNOTATED_ASSIGNMENT_EXPRESSION;
    }
}
