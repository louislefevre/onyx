package analysis.syntax;

import identifiers.StatementType;

import java.util.List;

public abstract class Statement
{
    public abstract StatementType getStatementType();

    public abstract List<Object> getChildren();
}
