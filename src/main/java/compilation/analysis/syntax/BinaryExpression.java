package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.ExpressionType;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static types.ExpressionType.BINARY_EXPRESSION;

/**
 * The BinaryExpression class is used to store information about binary expressions declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class BinaryExpression implements Expression
{
    private final Expression leftOperand;
    private final Token operatorToken;
    private final Expression rightOperand;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final ExpressionType expressionType;

    /**
     * Constructs a BinaryExpression object, initialised with the expressions contents.
     * <p>
     * A SourceSpan containing the range of the expression is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param leftOperand The left operand Expression
     * @param operatorToken The binary operator Token
     * @param rightOperand The right operand Expression
     */
    public BinaryExpression(Expression leftOperand, Token operatorToken, Expression rightOperand)
    {
        this.leftOperand = leftOperand;
        this.operatorToken = operatorToken;
        this.rightOperand = rightOperand;
        this.span = SourceSpan.inRange(leftOperand.getSpan().getStart(), rightOperand.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(leftOperand, operatorToken, rightOperand));
        this.expressionType = BINARY_EXPRESSION;
    }
}
