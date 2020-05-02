package analysis.syntax;

import analysis.lexical.Token;
import identifiers.StatementType;
import lombok.Getter;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

@Getter
public final class BlockStatement implements Statement
{
    private final Token openBraceToken;
    private final List<Statement> statements;
    private final Token closeBraceToken;
    private final StatementType statementType;
    private final Queue<Object> children;

    public BlockStatement(Token openBraceToken, List<Statement> statements, Token closeBraceToken)
    {
        this.openBraceToken = openBraceToken;
        this.statements = statements;
        this.closeBraceToken = closeBraceToken;
        this.statementType = StatementType.BLOCK_STATEMENT;
        this.children = new LinkedList<>(Arrays.asList(openBraceToken, statements, closeBraceToken));
    }
}
