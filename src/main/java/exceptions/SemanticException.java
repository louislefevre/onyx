package exceptions;

import static types.ExceptionType.SEMANTIC_EXCEPTION;

public class SemanticException extends Exception
{
    public SemanticException(String message)
    {
        super(SEMANTIC_EXCEPTION, message);
    }

    public static String undefinedExpression(String syntax)
    {
        return String.format("Unexpected expression '%s'", syntax);
    }

    public static String undefinedStatement(String syntax)
    {
        return String.format("Unexpected statement '%s'", syntax);
    }
}
