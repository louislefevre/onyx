package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedStatementType;

import java.util.List;

import static types.AnnotatedStatementType.ANNOTATED_SOURCE_STATEMENT;

@Getter
public final class AnnotatedSourceStatement implements AnnotatedStatement
{
    private final List<AnnotatedStatement> statements;
    private final AnnotatedStatementType statementType;

    public AnnotatedSourceStatement(List<AnnotatedStatement> statements)
    {
        this.statements = statements;
        this.statementType = ANNOTATED_SOURCE_STATEMENT;
    }
}
