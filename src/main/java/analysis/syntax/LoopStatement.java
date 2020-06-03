package analysis.syntax;

import analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.StatementType;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static types.StatementType.LOOP_STATEMENT;

@Getter
public final class LoopStatement implements Statement
{
    private final Token loopToken;
    private final Expression lowerBound;
    private final Token toToken;
    private final Expression upperBound;
    private final Statement body;
    private final StatementType statementType;
    private final SourceSpan span;
    private final Queue<Object> children;

    public LoopStatement(Token loopToken, Expression lowerBound, Token toToken, Expression upperBound, Statement body)
    {
        this.loopToken = loopToken;
        this.lowerBound = lowerBound;
        this.toToken = toToken;
        this.upperBound = upperBound;
        this.body = body;
        this.statementType = LOOP_STATEMENT;
        this.span = SourceSpan.inRange(loopToken.getSpan().getStart(), upperBound.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(loopToken, lowerBound, toToken, upperBound, body));
    }
}
