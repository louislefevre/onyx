package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedStatementType;

import static types.AnnotatedStatementType.ANNOTATED_EXPRESSION_STATEMENT;

/**
 * The AnnotatedExpressionStatement class is used to store information about annotated expression statements declared
 * during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedExpressionStatement implements AnnotatedStatement
{
    private final AnnotatedExpression expression;
    private final AnnotatedStatementType statementType;

    /**
     * Constructs an AnnotatedExpressionStatement object, initialised with the statements contents.
     *
     * @param expression The AnnotatedExpression object
     */
    public AnnotatedExpressionStatement(AnnotatedExpression expression)
    {
        this.expression = expression;
        this.statementType = ANNOTATED_EXPRESSION_STATEMENT;
    }
}
