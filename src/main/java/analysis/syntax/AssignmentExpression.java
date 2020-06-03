package analysis.syntax;

import analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.ExpressionType;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static types.ExpressionType.ASSIGNMENT_EXPRESSION;

@Getter
public final class AssignmentExpression implements Expression
{
    private final IdentifierExpression identifierExpression;
    private final Token assignmentToken;
    private final Expression expression;
    private final ExpressionType expressionType;
    private final SourceSpan span;
    private final Queue<Object> children;

    public AssignmentExpression(IdentifierExpression identifierExpression, Token assignmentToken, Expression expression)
    {
        this.identifierExpression = identifierExpression;
        this.assignmentToken = assignmentToken;
        this.expression = expression;
        this.expressionType = ASSIGNMENT_EXPRESSION;
        this.span = SourceSpan.inRange(identifierExpression.getSpan().getStart(), expression.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(identifierExpression, assignmentToken, expression));
    }
}
