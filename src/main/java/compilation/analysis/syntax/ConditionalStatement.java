package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.StatementType;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static types.StatementType.CONDITIONAL_STATEMENT;
import static types.StatementType.ELSE_STATEMENT;

/**
 * The ConditionalStatement class is used to store information about conditional statements declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class ConditionalStatement implements Statement
{
    private final Token ifToken;
    private final Expression condition;
    private final Statement thenStatement;
    private ElseStatement elseStatement;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final StatementType statementType;

    /**
     * Constructs a ConditionalStatement object, initialised with the statements contents.
     * <p>
     * A SourceSpan containing the range of the statement is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param ifToken The if Token
     * @param condition The condition Expression for the then Statement to run
     * @param thenStatement The Statement to be run if the condition is true
     */
    public ConditionalStatement(Token ifToken, Expression condition, Statement thenStatement)
    {
        this.ifToken = ifToken;
        this.condition = condition;
        this.thenStatement = thenStatement;
        this.elseStatement = null;
        this.span = SourceSpan.inRange(ifToken.getSpan().getStart(), condition.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(ifToken, condition, thenStatement));
        this.statementType = CONDITIONAL_STATEMENT;
    }

    /**
     * Adds an ElseStatement to this ConditionalStatement, which will run if the condition is false.
     *
     * @param elseToken The else Token
     * @param statement The Statement to be run if the condition is false
     */
    public void addElseStatement(Token elseToken, Statement statement)
    {
        elseStatement = new ElseStatement(elseToken, statement);
        children.add(elseStatement);
    }

    /**
     * Returns a boolean to indicate if a ConditionalStatement contains an ElseStatement.
     *
     * @return A boolean indicating if an else is present
     */
    public boolean includesElseStatement()
    {
        return elseStatement != null;
    }

    /**
     * The ElseStatement class is used to store information about else statements declared during compilation.
     *
     * @author Louis Lefevre
     * @version 1.0
     * @since 1.0
     */
    @Getter
    public static class ElseStatement implements Statement
    {
        private final Token elseToken;
        private final Statement statement;
        private final SourceSpan span;
        private final Queue<Object> children;
        private final StatementType statementType;

        /**
         * Constructs an ElseStatement object, initialised with the statements contents.
         * <p>
         * A SourceSpan containing the range of the statement is automatically generated, as well as a LinkedList of
         * its children.
         *
         * @param elseToken The else Token
         * @param statement The Statement to be run if the condition is false
         */
        private ElseStatement(Token elseToken, Statement statement)
        {
            this.elseToken = elseToken;
            this.statement = statement;
            this.span = SourceSpan.inRange(elseToken.getSpan().getStart(), elseToken.getSpan().getEnd());
            this.children = new LinkedList<>(Arrays.asList(elseToken, statement));
            this.statementType = ELSE_STATEMENT;
        }
    }
}
