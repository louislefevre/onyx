package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.ExpressionType;

import java.util.Collections;
import java.util.LinkedList;
import java.util.Queue;

import static types.ExpressionType.IDENTIFIER_EXPRESSION;

/**
 * The IdentifierExpression class is used to store information about identifier expressions declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class IdentifierExpression implements Expression
{
    private final Token identifierToken;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final ExpressionType expressionType;

    /**
     * Constructs an IdentifierExpression object, initialised with the expressions contents.
     * <p>
     * A SourceSpan containing the range of the expression is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param identifierToken The identifier Token
     */
    public IdentifierExpression(Token identifierToken)
    {
        this.identifierToken = identifierToken;
        this.span = SourceSpan.inRange(identifierToken.getSpan().getStart(), identifierToken.getSpan().getEnd());
        this.children = new LinkedList<>(Collections.singletonList(identifierToken));
        this.expressionType = IDENTIFIER_EXPRESSION;
    }
}
