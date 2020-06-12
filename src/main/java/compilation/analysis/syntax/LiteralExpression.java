package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.ExpressionType;

import java.util.Collections;
import java.util.LinkedList;
import java.util.Queue;

import static types.ExpressionType.LITERAL_EXPRESSION;

/**
 * The LiteralExpression class is used to store information about literal expressions declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class LiteralExpression implements Expression
{
    private final Token literalToken;
    private final Object value;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final ExpressionType expressionType;

    /**
     * Constructs a LiteralExpression object, initialised with the expressions contents.
     * <p>
     * A SourceSpan containing the range of the expression is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param literalToken The literal Token
     * @param value The value of the literal Token
     */
    public LiteralExpression(Token literalToken, Object value)
    {
        this.literalToken = literalToken;
        this.value = value;
        this.span = SourceSpan.inRange(literalToken.getSpan().getStart(), literalToken.getSpan().getEnd());
        this.children = new LinkedList<>(Collections.singletonList(literalToken));
        this.expressionType = LITERAL_EXPRESSION;
    }
}
