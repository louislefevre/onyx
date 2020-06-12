package compilation.analysis.syntax;

import compilation.analysis.lexical.Token;
import lombok.Getter;
import source.SourceSpan;
import types.StatementType;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import static types.StatementType.BLOCK_STATEMENT;

/**
 * The BlockStatement class is used to store information about block statements declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class BlockStatement implements Statement
{
    private final Token openBraceToken;
    private final List<Statement> statements;
    private final Token closeBraceToken;
    private final SourceSpan span;
    private final Queue<Object> children;
    private final StatementType statementType;

    /**
     * Constructs a BlockStatement object, initialised with the statements contents.
     * <p>
     * A SourceSpan containing the range of the statement is automatically generated, as well as a LinkedList of
     * its children.
     *
     * @param openBraceToken The open brace Token
     * @param statements The List of Statements within the braces
     * @param closeBraceToken The close brace Token
     */
    public BlockStatement(Token openBraceToken, List<Statement> statements, Token closeBraceToken)
    {
        this.openBraceToken = openBraceToken;
        this.statements = statements;
        this.closeBraceToken = closeBraceToken;
        this.span = SourceSpan.inRange(openBraceToken.getSpan().getStart(), closeBraceToken.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(openBraceToken, statements, closeBraceToken));
        this.statementType = BLOCK_STATEMENT;
    }
}
