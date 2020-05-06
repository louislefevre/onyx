package analysis.syntax;

import identifiers.StatementType;
import source.SourceSpan;

import java.util.Queue;

public interface Statement
{
    StatementType getStatementType();

    SourceSpan getSpan();

    Queue<Object> getChildren();
}
