package compilation.analysis.syntax;

import source.SourceSpan;
import types.StatementType;

import java.util.Queue;

/**
 * The Statement interface is used to represent statements declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public interface Statement
{
    /**
     * Return the SourceSpan of a Statement object
     *
     * @return the span of the Statement
     */
    SourceSpan getSpan();

    /**
     * Return the children of a Statement object
     *
     * @return the children of the Statement
     */
    Queue<Object> getChildren();

    /**
     * Return the StatementType of a Statement object
     *
     * @return the type of the Statement
     */
    StatementType getStatementType();
}
