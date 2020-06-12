package compilation.analysis.semantic;

import types.AnnotatedStatementType;

/**
 * The AnnotatedStatement interface is used to represent annotated statements declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public interface AnnotatedStatement
{
    /**
     * Return the AnnotatedStatementType of an AnnotatedStatement object
     *
     * @return the type of the Statement
     */
    AnnotatedStatementType getStatementType();
}
