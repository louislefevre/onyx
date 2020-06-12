package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.StatementType;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static types.StatementType.LOOP_STATEMENT;

/**
 * The LoopStatement class is used to store information about loop statements declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class LoopStatement implements Statement
{
    private final Token loopToken;
    private final Expression lowerBound;
    private final Token toToken;
    private final Expression upperBound;
    private final Statement body;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final StatementType statementType;

    /**
     * Constructs a LoopStatement object, initialised with the statements contents.
     * <p>
     * A SourceSpan containing the range of the statement is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param loopToken The loop Token
     * @param lowerBound The lower bound Expression
     * @param toToken The to Token
     * @param upperBound The upper bound Expression
     * @param body The Statement to be run in the loop body
     */
    public LoopStatement(Token loopToken, Expression lowerBound, Token toToken, Expression upperBound, Statement body)
    {
        this.loopToken = loopToken;
        this.lowerBound = lowerBound;
        this.toToken = toToken;
        this.upperBound = upperBound;
        this.body = body;
        this.span = SourceSpan.inRange(loopToken.getSpan().getStart(), upperBound.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(loopToken, lowerBound, toToken, upperBound, body));
        this.statementType = LOOP_STATEMENT;
    }
}
