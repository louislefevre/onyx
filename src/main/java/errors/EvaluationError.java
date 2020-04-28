package errors;

import identifiers.ErrorType;
import lombok.Getter;
import source.SourceSpan;

@Getter
public final class EvaluationError extends Error
{
    private final ErrorType errorType;

    public EvaluationError(SourceSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.EVALUATE_ERROR;
    }

    public static String exceptionOccurred(Exception exception)
    {
        StackTraceElement stackTraceElement = exception.getStackTrace()[0];
        int lineNumber = stackTraceElement.getLineNumber();
        String className = stackTraceElement.getClassName();

        String location = String.format("Line %1s: Exception occurred at %2s", lineNumber, className);
        String message = exception.getMessage();

        return location + "\n" + message;
    }

    public static Exception unexpectedExpression(String expression)
    {
        String message = String.format("Unexpected expression '%s'.", expression);
        return new Exception(message);
    }

    public static Exception unexpectedStatement(String statement)
    {
        String message = String.format("Unexpected statement '%s'.", statement);
        return new Exception(message);
    }

    public static Exception unexpectedUnaryObjectType(String type)
    {
        String message = String.format("Unexpected unary object type '%s'.", type);
        return new Exception(message);
    }

    public static Exception unexpectedBinaryObjectTypes(String leftType, String rightType)
    {
        String message = String.format("Unexpected binary object types '%1s' and '%2s'.", leftType, rightType);
        return new Exception(message);
    }

    public static Exception unexpectedUnaryOperator(String operator)
    {
        String message = String.format("Unexpected unary operator '%s'.", operator);
        return new Exception(message);
    }

    public static Exception unexpectedBinaryOperator(String operator)
    {
        String message = String.format("Unexpected binary operator '%s'.", operator);
        return new Exception(message);
    }

    public static Exception missingSymbol(String name)
    {
        String message = String.format("Symbol '%s' does not exist in symbol table.", name);
        return new Exception(message);
    }

    @Override
    public String toString()
    {
        return "Evaluate Error";
    }
}
