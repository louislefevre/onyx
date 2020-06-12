package compilation.analysis.syntax;

import source.SourceSpan;
import types.ExpressionType;

import java.util.Queue;

/**
 * The Expression interface is used to represent expressions declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public interface Expression
{
    /**
     * Return the SourceSpan of an Expression object
     *
     * @return the span of the Expression
     */
    SourceSpan getSpan();

    /**
     * Return the children of an Expression object
     *
     * @return the children of the Expression
     */
    Queue<Object> getChildren();

    /**
     * Return the ExpressionType of an Expression object
     *
     * @return the type of the Expression
     */
    ExpressionType getExpressionType();
}
