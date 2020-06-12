package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedExpressionType;
import types.ObjectType;

import static types.AnnotatedExpressionType.ANNOTATED_BINARY_EXPRESSION;

/**
 * The AnnotatedBinaryExpression class is used to store information about annotated binary expressions declared
 * during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedBinaryExpression implements AnnotatedExpression
{
    private final AnnotatedExpression leftOperand;
    private final AnnotatedBinaryOperator operator;
    private final AnnotatedExpression rightOperand;
    private final ObjectType objectType;
    private final AnnotatedExpressionType expressionType;

    /**
     * Constructs an AnnotatedBinaryExpression object, initialised with the expressions contents.
     * <p>
     * The ObjectType is automatically added based on the result type of the operator.
     *
     * @param leftOperand The left operand
     * @param operator The binary operator
     * @param rightOperand The right operand
     */
    public AnnotatedBinaryExpression(AnnotatedExpression leftOperand, AnnotatedBinaryOperator operator,
                                     AnnotatedExpression rightOperand)
    {
        this.leftOperand = leftOperand;
        this.operator = operator;
        this.rightOperand = rightOperand;
        this.objectType = operator.getResultObjectType();
        this.expressionType = ANNOTATED_BINARY_EXPRESSION;
    }
}
