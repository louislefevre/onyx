package analysis.syntax;

import identifiers.StatementType;

import java.util.Queue;

public interface Statement
{
    StatementType getStatementType();

    Queue<Object> getChildren();
}
