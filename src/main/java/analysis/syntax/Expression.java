package analysis.syntax;

import identifiers.ExpressionType;
import source.SourceSpan;

import java.util.Queue;

public interface Expression
{
    ExpressionType getExpressionType();

    SourceSpan getSpan();

    Queue<Object> getChildren();
}
