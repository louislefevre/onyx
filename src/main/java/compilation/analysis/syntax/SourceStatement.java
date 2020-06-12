package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.StatementType;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import static types.StatementType.SOURCE_STATEMENT;

/**
 * The SourceStatement class is used to store information about the entire source code during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class SourceStatement implements Statement
{
    private final List<Statement> statements;
    private final Token endToken;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final StatementType statementType;

    /**
     * Constructs a SourceStatement object, initialised with a List of every Statement object within the source code
     * and the EOF Token.
     * <p>
     * A SourceSpan containing the range of the source code is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param statements The List of Statements within the source code
     * @param endToken The end of file Token
     */
    public SourceStatement(List<Statement> statements, Token endToken)
    {
        this.statements = statements;
        this.endToken = endToken;
        this.span = SourceSpan.inRange(0, endToken.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(statements, endToken));
        this.statementType = SOURCE_STATEMENT;
    }

    /**
     * Constructs a SourceStatement object, initialised with a single Statement object and the EOF Token.
     * <p>
     * A SourceSpan containing the range of the statement is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param statement The Statement object
     * @param endToken The end of file Token
     */
    public SourceStatement(Statement statement, Token endToken)
    {
        this(Collections.singletonList(statement), endToken);
    }
}
