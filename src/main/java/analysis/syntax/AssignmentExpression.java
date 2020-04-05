package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;
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
    private final TokenType tokenType;
    private final List<Object> children;

    public AssignmentExpression(Token identifierToken, Token equalsToken, Expression expression)
    {
        this.identifierToken = identifierToken;
        this.equalsToken = equalsToken;
        this.expression = expression;
        this.tokenType = TokenType.ASSIGNMENT_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Arrays.asList(identifierToken, equalsToken, expression));
    }
}
