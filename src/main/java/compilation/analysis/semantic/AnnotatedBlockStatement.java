package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedStatementType;

import java.util.List;

import static types.AnnotatedStatementType.ANNOTATED_BLOCK_STATEMENT;

/**
 * The AnnotatedBlockStatement class is used to store information about annotated block statements declared during
 * compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedBlockStatement implements AnnotatedStatement
{
    private final List<AnnotatedStatement> statements;
    private final AnnotatedStatementType statementType;

    /**
     * Constructs an AnnotatedBlockStatement object, initialised with the statements contents.
     *
     * @param statements The List of Statements within the braces
     */
    public AnnotatedBlockStatement(List<AnnotatedStatement> statements)
    {
        this.statements = statements;
        this.statementType = ANNOTATED_BLOCK_STATEMENT;
    }
}
