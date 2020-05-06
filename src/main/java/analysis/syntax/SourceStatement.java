package analysis.syntax;

import analysis.lexical.Token;
import identifiers.StatementType;
import lombok.Getter;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import static identifiers.StatementType.SOURCE_STATEMENT;

@Getter
public final class SourceStatement implements Statement
{
    private final List<Statement> statements;
    private final Token endToken;
    private final StatementType statementType;
    private final Queue<Object> children;

    public SourceStatement(List<Statement> statements, Token endToken)
    {
        this.statements = statements;
        this.endToken = endToken;
        this.statementType = SOURCE_STATEMENT;
        this.children = new LinkedList<>(Arrays.asList(statements, endToken));
    }
}
