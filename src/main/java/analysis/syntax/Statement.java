package analysis.syntax;

import source.SourceSpan;
import types.StatementType;

import java.util.Queue;

public interface Statement
{
    StatementType getStatementType();

    SourceSpan getSpan();

    Queue<Object> getChildren();
}
