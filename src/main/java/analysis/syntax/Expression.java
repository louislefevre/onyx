package analysis.syntax;

import identifiers.ExpressionType;

import java.util.List;

public interface Expression
{
    ExpressionType getExpressionType();

    List<Object> getChildren();
}
