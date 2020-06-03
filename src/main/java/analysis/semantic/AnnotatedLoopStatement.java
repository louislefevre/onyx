package analysis.semantic;

import lombok.Getter;
import types.AnnotatedStatementType;

import static types.AnnotatedStatementType.ANNOTATED_LOOP_STATEMENT;

@Getter
public final class AnnotatedLoopStatement implements AnnotatedStatement
{
    private final AnnotatedExpression lowerBound;
    private final AnnotatedExpression upperBound;
    private final AnnotatedStatement body;
    private final AnnotatedStatementType statementType;

    public AnnotatedLoopStatement(AnnotatedExpression lowerBound, AnnotatedExpression upperBound, AnnotatedStatement body)
    {
        this.lowerBound = lowerBound;
        this.upperBound = upperBound;
        this.body = body;
        this.statementType = ANNOTATED_LOOP_STATEMENT;
    }
}
