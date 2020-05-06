package analysis.syntax;

import identifiers.StatementType;
import lombok.Getter;
import source.SourceSpan;

import java.util.Collections;
import java.util.LinkedList;
import java.util.Queue;

import static identifiers.StatementType.EXPRESSION_STATEMENT;

@Getter
public final class ExpressionStatement implements Statement
{
    private final Expression expression;
    private final StatementType statementType;
    private final SourceSpan span;
    private final Queue<Object> children;

    public ExpressionStatement(Expression expression)
    {
        this.expression = expression;
        this.statementType = EXPRESSION_STATEMENT;
        this.span = SourceSpan.inRange(expression.getSpan().getStart(), expression.getSpan().getEnd());
        this.children = new LinkedList<>(Collections.singletonList(expression));
    }
}
