package analysis.syntax;

import source.SourceSpan;
import types.ExpressionType;

import java.util.Queue;

public interface Expression
{
    ExpressionType getExpressionType();

    SourceSpan getSpan();

    Queue<Object> getChildren();
}
