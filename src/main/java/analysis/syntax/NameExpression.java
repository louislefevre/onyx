package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Getter
public final class NameExpression extends Expression
{
    private final Token identifierToken;
    private final ExpressionType expressionType;
    private final List<Object> children;

    public NameExpression(Token identifierToken)
    {
        this.identifierToken = identifierToken;
        this.expressionType = ExpressionType.NAME_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Collections.singletonList(identifierToken));
    }
}
