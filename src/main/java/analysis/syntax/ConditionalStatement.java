package analysis.syntax;

import analysis.lexical.Token;
import identifiers.StatementType;
import lombok.Getter;
import source.SourceSpan;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static identifiers.StatementType.CONDITIONAL_STATEMENT;
import static identifiers.StatementType.ELSE_STATEMENT;

@Getter
public final class ConditionalStatement implements Statement
{
    private final Token ifToken;
    private final Expression condition;
    private final Statement thenStatement;
    private ElseStatement elseStatement;
    private final StatementType statementType;
    private final SourceSpan span;
    private final Queue<Object> children;

    public ConditionalStatement(Token ifToken, Expression condition, Statement thenStatement)
    {
        this.ifToken = ifToken;
        this.condition = condition;
        this.thenStatement = thenStatement;
        this.elseStatement = null;
        this.statementType = CONDITIONAL_STATEMENT;
        this.span = SourceSpan.inRange(ifToken.getSpan().getStart(), condition.getSpan().getEnd());
        this.children = new LinkedList<>(Arrays.asList(ifToken, condition, thenStatement));
    }

    public void addElseStatement(Token elseToken, Statement statement)
    {
        elseStatement = new ElseStatement(elseToken, statement);
        children.add(elseStatement);
    }

    public boolean includesElseStatement()
    {
        return elseStatement != null;
    }

    @Getter
    public class ElseStatement implements Statement
    {
        private final Token elseToken;
        private final Statement statement;
        private final StatementType statementType;
        private final SourceSpan span;
        private final Queue<Object> children;

        public ElseStatement(Token elseToken, Statement statement)
        {
            this.elseToken = elseToken;
            this.statement = statement;
            this.statementType = ELSE_STATEMENT;
            this.span = SourceSpan.inRange(elseToken.getSpan().getStart(), elseToken.getSpan().getEnd());
            this.children = new LinkedList<>(Arrays.asList(elseToken, statement));
        }
    }
}
