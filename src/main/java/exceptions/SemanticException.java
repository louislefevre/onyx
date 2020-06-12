package exceptions;

import types.ExpressionType;
import types.StatementType;

import static types.ExceptionType.SEMANTIC_EXCEPTION;

public final class SemanticException extends Exception
{
    public SemanticException(String message)
    {
        super(SEMANTIC_EXCEPTION, message);
    }

    /**
     * Generate and return a String message for an unexpected expression exception.
     *
     * @param type The type of the expression
     * @return A String containing the exception message
     */
    public static String unexpectedExpression(ExpressionType type)
    {
        return String.format("Unexpected expression '%s'", type.toString());
    }

    /**
     * Generate and return a String message for an unexpected statement exception.
     *
     * @param type The type of the statement
     * @return A String containing the exception message
     */
    public static String unexpectedStatement(StatementType type)
    {
        return String.format("Unexpected statement '%s'", type.toString());
    }
}
