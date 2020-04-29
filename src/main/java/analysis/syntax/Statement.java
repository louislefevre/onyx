package analysis.syntax;

import identifiers.StatementType;

import java.util.List;

public interface Statement
{
    StatementType getStatementType();

    List<Object> getChildren();
}
