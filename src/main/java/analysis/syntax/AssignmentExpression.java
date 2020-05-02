package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

@Getter
public final class AssignmentExpression implements Expression
{
    private final Token identifierToken;
    private final Token assignmentToken;
    private final Expression expression;
    private final ExpressionType expressionType;
    private final Queue<Object> children;

    public AssignmentExpression(Token identifierToken, Token assignmentToken, Expression expression)
    {
        this.identifierToken = identifierToken;
        this.assignmentToken = assignmentToken;
        this.expression = expression;
        this.expressionType = ExpressionType.ASSIGNMENT_EXPRESSION;
        this.children = new LinkedList<>(Arrays.asList(identifierToken, assignmentToken, expression));
    }
}
