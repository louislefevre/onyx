package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedExpressionType;
import types.ObjectType;

import static types.AnnotatedExpressionType.ANNOTATED_ASSIGNMENT_EXPRESSION;

/**
 * The AnnotatedAssignmentExpression class is used to store information about annotated assignment expressions declared
 * during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedAssignmentExpression implements AnnotatedExpression
{
    private final AnnotatedIdentifierExpression identifier;
    private final AnnotatedAssignmentOperator operator;
    private final AnnotatedExpression expression;
    private final ObjectType objectType;
    private final AnnotatedExpressionType expressionType;

    /**
     * Constructs an AnnotatedAssignmentExpression object, initialised with the expressions contents.
     * <p>
     * The ObjectType is automatically added based on the type of the assigned Expression.
     *
     * @param identifier The identifier being assigned to
     * @param operator The operator being used
     * @param expression The expression being assigned
     */
    public AnnotatedAssignmentExpression(AnnotatedIdentifierExpression identifier, AnnotatedAssignmentOperator operator, AnnotatedExpression expression)
    {
        this.identifier = identifier;
        this.operator = operator;
        this.expression = expression;
        this.objectType = expression.getObjectType();
        this.expressionType = ANNOTATED_ASSIGNMENT_EXPRESSION;
    }
}
