package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.ExpressionType;

import java.util.Collections;
import java.util.LinkedList;
import java.util.Queue;

import static types.ExpressionType.IDENTIFIER_EXPRESSION;

@Getter
public final class IdentifierExpression implements Expression
{
    private final Token identifierToken;
    private final ExpressionType expressionType;
    private final SourceSpan span;
    private final Queue<Object> children;

    public IdentifierExpression(Token identifierToken)
    {
        this.identifierToken = identifierToken;
        this.expressionType = IDENTIFIER_EXPRESSION;
        this.span = SourceSpan.inRange(identifierToken.getSpan().getStart(), identifierToken.getSpan().getEnd());
        this.children = new LinkedList<>(Collections.singletonList(identifierToken));
    }
}