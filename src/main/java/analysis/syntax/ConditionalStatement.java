package analysis.syntax;

import analysis.lexical.Token;
import identifiers.StatementType;
import lombok.Getter;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

@Getter
public final class ConditionalStatement implements Statement
{
    private final Token ifToken;
    private final Expression conditionExpression;
    private final Statement thenStatement;
    private ElseStatement elseStatement;
    private final StatementType statementType;
    private final Queue<Object> children;

    public ConditionalStatement(Token ifToken, Expression conditionExpression, Statement thenStatement)
    {
        this.ifToken = ifToken;
        this.conditionExpression = conditionExpression;
        this.thenStatement = thenStatement;
        this.elseStatement = null;
        this.statementType = StatementType.CONDITIONAL_STATEMENT;
        this.children = new LinkedList<>(Arrays.asList(ifToken, conditionExpression, thenStatement));
    }

    public void addElseStatement(Token elseToken, Statement elseStatement)
    {
        this.elseStatement = new ElseStatement(elseToken, elseStatement);
        this.children.add(this.elseStatement);
    }

    public boolean includesElseStatement()
    {
        return this.elseStatement != null;
    }

    @Getter
    public class ElseStatement implements Statement
    {
        private final Token elseToken;
        private final Statement statement;
        private final StatementType statementType;
        private final Queue<Object> children;

        public ElseStatement(Token elseToken, Statement statement)
        {
            this.elseToken = elseToken;
            this.statement = statement;
            this.statementType = StatementType.ELSE_STATEMENT;
            this.children = new LinkedList<>(Arrays.asList(elseToken, statement));
        }
    }
}
