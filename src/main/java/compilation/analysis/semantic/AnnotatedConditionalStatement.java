package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedStatementType;

import static types.AnnotatedStatementType.ANNOTATED_CONDITIONAL_STATEMENT;

@Getter
public final class AnnotatedConditionalStatement implements AnnotatedStatement
{
    private final AnnotatedExpression condition;
    private final AnnotatedStatement thenStatement;
    private final AnnotatedStatement elseStatement;
    private final AnnotatedStatementType statementType;

    public AnnotatedConditionalStatement(AnnotatedExpression condition, AnnotatedStatement thenStatement,
                                         AnnotatedStatement elseStatement)
    {
        this.condition = condition;
        this.thenStatement = thenStatement;
        this.elseStatement = elseStatement;
        this.statementType = ANNOTATED_CONDITIONAL_STATEMENT;
    }

    public boolean includesElseStatement()
    {
        return elseStatement != null;
    }
}
