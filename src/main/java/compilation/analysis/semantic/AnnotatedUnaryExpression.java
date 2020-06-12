package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedExpressionType;
import types.ObjectType;

import static types.AnnotatedExpressionType.ANNOTATED_UNARY_EXPRESSION;

/**
 * The AnnotatedUnaryExpression class is used to store information about annotated unary expressions declared
 * during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedUnaryExpression implements AnnotatedExpression
{
    private final AnnotatedUnaryOperator operator;
    private final AnnotatedExpression operand;
    private final ObjectType objectType;
    private final AnnotatedExpressionType expressionType;

    /**
     * Constructs an AnnotatedUnaryExpression object, initialised with the expressions contents.
     * <p>
     * The ObjectType is automatically added based on the result type of the operator.
     *
     * @param operator The unary operator
     * @param operand The expression the operator is being applied to
     */
    public AnnotatedUnaryExpression(AnnotatedUnaryOperator operator, AnnotatedExpression operand)
    {
        this.operator = operator;
        this.operand = operand;
        this.objectType = operator.getResultObjectType();
        this.expressionType = ANNOTATED_UNARY_EXPRESSION;
    }
}
