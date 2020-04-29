package analysis.semantic;

import identifiers.AnnotatedStatementType;
import lombok.Getter;

import java.util.List;

@Getter
public final class AnnotatedBlockStatement implements AnnotatedStatement
{
    private final List<AnnotatedStatement> annotatedStatementList;
    private final AnnotatedStatementType annotatedStatementType;

    public AnnotatedBlockStatement(List<AnnotatedStatement> annotatedStatementList)
    {
        this.annotatedStatementList = annotatedStatementList;
        this.annotatedStatementType = AnnotatedStatementType.ANNOTATED_BLOCK_STATEMENT;
    }
}
