package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedStatementType;

import static types.AnnotatedStatementType.ANNOTATED_LOOP_STATEMENT;

/**
 * The AnnotatedLoopStatement class is used to store information about annotated loop statements declared during
 * compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedLoopStatement implements AnnotatedStatement
{
    private final AnnotatedExpression lowerBound;
    private final AnnotatedExpression upperBound;
    private final AnnotatedStatement body;
    private final AnnotatedStatementType statementType;

    /**
     * Constructs an AnnotatedLoopStatement object, initialised with the statements contents.
     *
     * @param lowerBound The lower bound AnnotatedExpression
     * @param upperBound The upper bound AnnotatedExpression
     * @param body The AnnotatedStatement to be run in the loop body
     */
    public AnnotatedLoopStatement(AnnotatedExpression lowerBound, AnnotatedExpression upperBound, AnnotatedStatement body)
    {
        this.lowerBound = lowerBound;
        this.upperBound = upperBound;
        this.body = body;
        this.statementType = ANNOTATED_LOOP_STATEMENT;
    }
}
