package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedStatementType;

import java.util.List;

import static types.AnnotatedStatementType.ANNOTATED_BLOCK_STATEMENT;

@Getter
public final class AnnotatedBlockStatement implements AnnotatedStatement
{
    private final List<AnnotatedStatement> statements;
    private final AnnotatedStatementType statementType;

    public AnnotatedBlockStatement(List<AnnotatedStatement> statements)
    {
        this.statements = statements;
        this.statementType = ANNOTATED_BLOCK_STATEMENT;
    }
}
