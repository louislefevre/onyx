package analysis.semantic;

import identifiers.AnnotatedStatementType;
import lombok.Getter;

import java.util.List;

@Getter
public final class AnnotatedBlockStatement implements AnnotatedStatement
{
    private final List<AnnotatedStatement> statements;
    private final AnnotatedStatementType statementType;

    public AnnotatedBlockStatement(List<AnnotatedStatement> statements)
    {
        this.statements = statements;
        this.statementType = AnnotatedStatementType.ANNOTATED_BLOCK_STATEMENT;
    }
}
