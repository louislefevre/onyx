package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.ExpressionType;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static types.ExpressionType.ASSIGNMENT_EXPRESSION;

/**
 * The AssignmentExpression class is used to store information about assignment expressions declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AssignmentExpression implements Expression
{
    private final IdentifierExpression identifierExpression;
    private final Token assignmentToken;
    private final Expression expression;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final ExpressionType expressionType;

    /**
     * Constructs an AssignmentExpression object, initialised with the expressions contents.
     * <p>
     * A SourceSpan containing the range of the expression is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param identifierExpression The IdentifierExpression being assigned to
     * @param assignmentToken The assignment operator Token
     * @param expression The Expression which is being assigned
     */
    public AssignmentExpression(IdentifierExpression identifierExpression, Token assignmentToken, Expression expression)
    {
        this.identifierExpression = identifierExpression;
        this.assignmentToken = assignmentToken;
        this.expression = expression;
        this.span = SourceSpan.inRange(identifierExpression.getSpan().getStart(), expression.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(identifierExpression, assignmentToken, expression));
        this.expressionType = ASSIGNMENT_EXPRESSION;
    }
}
