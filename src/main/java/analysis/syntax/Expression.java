package analysis.syntax;

import identifiers.ExpressionType;

import java.util.Queue;

public interface Expression
{
    ExpressionType getExpressionType();

    Queue<Object> getChildren();
}
