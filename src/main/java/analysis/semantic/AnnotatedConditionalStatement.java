package analysis.semantic;

import identifiers.AnnotatedStatementType;
import lombok.Getter;

@Getter
public final class AnnotatedConditionalStatement implements AnnotatedStatement
{
    private final AnnotatedExpression annotatedCondition;
    private final AnnotatedStatement annotatedThenStatement;
    private final AnnotatedStatement annotatedElseClause;
    private final AnnotatedStatementType statementType;

    public AnnotatedConditionalStatement(AnnotatedExpression annotatedCondition, AnnotatedStatement annotatedThenStatement,
                                         AnnotatedStatement annotatedElseClause)
    {
        this.annotatedCondition = annotatedCondition;
        this.annotatedThenStatement = annotatedThenStatement;
        this.annotatedElseClause = annotatedElseClause;
        this.statementType = AnnotatedStatementType.ANNOTATED_CONDITIONAL_STATEMENT;
    }

    public boolean includesElseStatement()
    {
        return this.annotatedElseClause != null;
    }
}
