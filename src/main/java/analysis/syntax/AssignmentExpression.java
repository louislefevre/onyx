package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Getter
public final class AssignmentExpression extends Expression
{
    private final Token identifierToken;
    private final Token equalsToken;
    private final Expression expression;
    private final ExpressionType expressionType;
    private final List<Object> children;

    public AssignmentExpression(Token identifierToken, Token equalsToken, Expression expression)
    {
        this.identifierToken = identifierToken;
        this.equalsToken = equalsToken;
        this.expression = expression;
        this.expressionType = ExpressionType.ASSIGNMENT_EXPRESSION;
        this.children = new ArrayList<>(Arrays.asList(identifierToken, equalsToken, expression));
    }
}
