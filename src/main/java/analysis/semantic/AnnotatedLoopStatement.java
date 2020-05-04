package analysis.semantic;

import identifiers.AnnotatedStatementType;
import lombok.Getter;
import symbols.Symbol;

import static identifiers.AnnotatedStatementType.ANNOTATED_LOOP_STATEMENT;

@Getter
public final class AnnotatedLoopStatement implements AnnotatedStatement
{
    private final Symbol symbol;
    private final AnnotatedExpression lowerBound;
    private final AnnotatedExpression upperBound;
    private final AnnotatedStatement body;
    private final AnnotatedStatementType statementType;

    public AnnotatedLoopStatement(Symbol symbol, AnnotatedExpression lowerBound, AnnotatedExpression upperBound, AnnotatedStatement body)
    {
        this.symbol = symbol;
        this.lowerBound = lowerBound;
        this.upperBound = upperBound;
        this.body = body;
        this.statementType = ANNOTATED_LOOP_STATEMENT;
    }
}
