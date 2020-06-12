package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedStatementType;

import static types.AnnotatedStatementType.ANNOTATED_CONDITIONAL_STATEMENT;

/**
 * The AnnotatedConditionalStatement class is used to store information about annotated conditional statements declared
 * during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedConditionalStatement implements AnnotatedStatement
{
    private final AnnotatedExpression condition;
    private final AnnotatedStatement thenStatement;
    private final AnnotatedStatement elseStatement;
    private final AnnotatedStatementType statementType;

    /**
     * Constructs an AnnotatedConditionalStatement object, initialised with the statements contents.
     *
     * @param condition The AnnotatedExpression condition for the then AnnotatedStatement to run
     * @param thenStatement The AnnotatedExpression to be run if the condition is true
     * @param elseStatement The AnnotatedExpression to be run if the condition is false
     */
    public AnnotatedConditionalStatement(AnnotatedExpression condition, AnnotatedStatement thenStatement,
                                         AnnotatedStatement elseStatement)
    {
        this.condition = condition;
        this.thenStatement = thenStatement;
        this.elseStatement = elseStatement;
        this.statementType = ANNOTATED_CONDITIONAL_STATEMENT;
    }

    /**
     * Returns a boolean to indicate if an AnnotatedConditionalStatement contains an ElseStatement.
     *
     * @return A boolean indicating if an else is present
     */
    public boolean includesElseStatement()
    {
        return elseStatement != null;
    }
}
