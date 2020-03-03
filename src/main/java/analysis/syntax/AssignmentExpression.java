package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class AssignmentExpression extends Expression
{
    @Getter private final Token identifierToken;
    @Getter private final Token equalsToken;
    @Getter private final Expression expression;
    @Getter private final TokenType tokenType;
    @Getter private final List<Object> children;

    public AssignmentExpression(Token identifierToken, Token equalsToken, Expression expression)
    {
        this.identifierToken = identifierToken;
        this.equalsToken = equalsToken;
        this.expression = expression;
        this.tokenType = TokenType.ASSIGNMENT_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Arrays.asList(identifierToken, equalsToken, expression));;
    }
}
