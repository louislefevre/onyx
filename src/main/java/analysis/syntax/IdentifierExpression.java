package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Getter
public final class IdentifierExpression implements Expression
{
    private final Token identifierToken;
    private final ExpressionType expressionType;
    private final List<Object> children;

    public IdentifierExpression(Token identifierToken)
    {
        this.identifierToken = identifierToken;
        this.expressionType = ExpressionType.IDENTIFIER_EXPRESSION;
        this.children = new ArrayList<>(Collections.singletonList(identifierToken));
    }
}
