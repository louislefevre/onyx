package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.Collections;
import java.util.LinkedList;
import java.util.Queue;

import static identifiers.ExpressionType.LITERAL_EXPRESSION;

@Getter
public final class LiteralExpression implements Expression
{
    private final Token literalToken;
    private final Object value;
    private final ExpressionType expressionType;
    private final Queue<Object> children;

    public LiteralExpression(Token literalToken, Object value)
    {
        this.literalToken = literalToken;
        this.value = value;
        this.expressionType = LITERAL_EXPRESSION;
        this.children = new LinkedList<>(Collections.singletonList(literalToken));
    }
}
