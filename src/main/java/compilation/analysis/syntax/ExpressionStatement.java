package compilation.analysis.syntax;

import lombok.Getter;
import source.SourceSpan;
import types.StatementType;

import java.util.Collections;
import java.util.LinkedList;
import java.util.Queue;

import static types.StatementType.EXPRESSION_STATEMENT;

/**
 * The ExpressionStatement class is used to store information about expression statements declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class ExpressionStatement implements Statement
{
    private final Expression expression;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final StatementType statementType;

    /**
     * Constructs an ExpressionStatement object, initialised with the statements contents.
     * <p>
     * A SourceSpan containing the range of the statement is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param expression The Expression object
     */
    public ExpressionStatement(Expression expression)
    {
        this.expression = expression;
        this.span = SourceSpan.inRange(expression.getSpan().getStart(), expression.getSpan().getEnd());
        this.children = new LinkedList<>(Collections.singletonList(expression));
        this.statementType = EXPRESSION_STATEMENT;
    }
}
