package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedStatementType;

import java.util.List;

import static types.AnnotatedStatementType.ANNOTATED_SOURCE_STATEMENT;

/**
 * The AnnotatedSourceStatement class is used to store information about the entire source code once its been annotated
 * during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedSourceStatement implements AnnotatedStatement
{
    private final List<AnnotatedStatement> statements;
    private final AnnotatedStatementType statementType;

    /**
     * Constructs a SourceStatement object, initialised with a List of every AnnotatedStatement object within the
     * source code.
     *
     * @param statements The List of AnnotatedStatements within the source code
     */
    public AnnotatedSourceStatement(List<AnnotatedStatement> statements)
    {
        this.statements = statements;
        this.statementType = ANNOTATED_SOURCE_STATEMENT;
    }
}
