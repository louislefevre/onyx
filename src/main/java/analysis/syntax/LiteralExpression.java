package analysis.syntax;

import analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.ExpressionType;

import java.util.Collections;
import java.util.LinkedList;
import java.util.Queue;

import static types.ExpressionType.LITERAL_EXPRESSION;

@Getter
public final class LiteralExpression implements Expression
{
    private final Token literalToken;
    private final Object value;
    private final ExpressionType expressionType;
    private final SourceSpan span;
    private final Queue<Object> children;

    public LiteralExpression(Token literalToken, Object value)
    {
        this.literalToken = literalToken;
        this.value = value;
        this.expressionType = LITERAL_EXPRESSION;
        this.span = SourceSpan.inRange(literalToken.getSpan().getStart(), literalToken.getSpan().getEnd());
        this.children = new LinkedList<>(Collections.singletonList(literalToken));
    }
}
