package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.ExpressionType;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static types.ExpressionType.UNARY_EXPRESSION;

/**
 * The UnaryExpression class is used to store information about unary expressions declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class UnaryExpression implements Expression
{
    private final Token operatorToken;
    private final Expression operand;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final ExpressionType expressionType;

    /**
     * Constructs a UnaryExpression object, initialised with the expressions contents.
     * <p>
     * A SourceSpan containing the range of the expression is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param operatorToken The unary operator Token
     * @param operand The Expression the operator is being applied to
     */
    public UnaryExpression(Token operatorToken, Expression operand)
    {
        this.operatorToken = operatorToken;
        this.operand = operand;
        this.span = SourceSpan.inRange(operatorToken.getSpan().getStart(), operand.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(operatorToken, operand));
        this.expressionType = UNARY_EXPRESSION;
    }
}
