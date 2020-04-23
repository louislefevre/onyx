package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Getter
public final class LiteralExpression extends Expression
{
    private final Token literalToken;
    private final Object value;
    private final ExpressionType expressionType;
    private final List<Object> children;

    public LiteralExpression(Token literalToken, Object value)
    {
        this.literalToken = literalToken;
        this.value = value;
        this.expressionType = ExpressionType.LITERAL_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Collections.singletonList(literalToken));
    }
}
