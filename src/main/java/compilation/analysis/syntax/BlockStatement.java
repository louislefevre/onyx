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

@Getter
public final class BlockStatement implements Statement
{
    private final Token openBraceToken;
    private final List<Statement> statements;
    private final Token closeBraceToken;
    private final StatementType statementType;
    private final SourceSpan span;
    private final Queue<Object> children;

    public BlockStatement(Token openBraceToken, List<Statement> statements, Token closeBraceToken)
    {
        this.openBraceToken = openBraceToken;
        this.statements = statements;
        this.closeBraceToken = closeBraceToken;
        this.statementType = BLOCK_STATEMENT;
        this.span = SourceSpan.inRange(openBraceToken.getSpan().getStart(), closeBraceToken.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(openBraceToken, statements, closeBraceToken));
    }
}
