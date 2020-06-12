package compilation.analysis.semantic;

import types.ObjectType;
import types.OperatorType;
import types.TokenType;

/**
 * The AnnotatedOperator interface is used to represent annotated operators declared during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public interface AnnotatedOperator
{
    /**
     * Return the TokenType of an AnnotatedOperator object
     *
     * @return the TokenType of the AnnotatedOperator
     */
    TokenType getTokenType();

    /**
     * Return the OperatorType of an AnnotatedOperator object
     *
     * @return the OperatorType of the AnnotatedOperator
     */
    OperatorType getOperatorType();

    /**
     * Return the ObjectType of an AnnotatedOperator object
     *
     * @return the ObjectType of the AnnotatedOperator
     */
    ObjectType getResultObjectType();
}
