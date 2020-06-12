package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.ExpressionType;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static types.ExpressionType.PARENTHESIZED_EXPRESSION;

/**
 * The ParenthesizedExpression class is used to store information about parenthesized expressions declared during
 * compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class ParenthesizedExpression implements Expression
{
    private final Token openParenthesisToken;
    private final Expression expression;
    private final Token closeParenthesisToken;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final ExpressionType expressionType;

    /**
     * Constructs an ParenthesizedExpression object, initialised with the expressions contents.
     * <p>
     * A SourceSpan containing the range of the expression is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param openParenthesisToken The open parenthesis Token
     * @param expression The Expression within the parenthesis
     * @param closeParenthesisToken The close parenthesis Token
     */
    public ParenthesizedExpression(Token openParenthesisToken, Expression expression, Token closeParenthesisToken)
    {
        this.openParenthesisToken = openParenthesisToken;
        this.expression = expression;
        this.closeParenthesisToken = closeParenthesisToken;
        this.span = SourceSpan.inRange(openParenthesisToken.getSpan().getStart(), closeParenthesisToken.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(openParenthesisToken, expression, closeParenthesisToken));
        this.expressionType = PARENTHESIZED_EXPRESSION;
    }
}
