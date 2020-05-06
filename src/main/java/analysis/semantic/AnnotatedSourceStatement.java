package analysis.semantic;

import identifiers.AnnotatedStatementType;
import lombok.Getter;

import java.util.List;

import static identifiers.AnnotatedStatementType.ANNOTATED_SOURCE_STATEMENT;

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
