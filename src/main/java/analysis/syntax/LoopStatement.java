package analysis.syntax;

import analysis.lexical.Token;
import identifiers.StatementType;
import lombok.Getter;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static identifiers.StatementType.LOOP_STATEMENT;

@Getter
public final class LoopStatement implements Statement
{
    private final Token loopToken;
    private final Token identifierToken;
    private final Token equalsToken;
    private final Expression lowerBound;
    private final Token toToken;
    private final Expression upperBound;
    private final Statement body;
    private final StatementType statementType;
    private final Queue<Object> children;

    public LoopStatement(Token loopToken, Token identifierToken, Token equalsToken, Expression lowerBound,
                         Token toToken, Expression upperBound, Statement body)
    {
        this.loopToken = loopToken;
        this.identifierToken = identifierToken;
        this.equalsToken = equalsToken;
        this.lowerBound = lowerBound;
        this.toToken = toToken;
        this.upperBound = upperBound;
        this.body = body;
        this.statementType = LOOP_STATEMENT;
        this.children = new LinkedList<>(Arrays.asList(loopToken, identifierToken, equalsToken,
                                                       lowerBound, toToken, upperBound, body));
    }
}
