package analysis.syntax;

import identifiers.ExpressionType;

import java.util.List;

public abstract class Expression
{
    public abstract ExpressionType getExpressionType();

    public abstract List<Object> getChildren();
}
