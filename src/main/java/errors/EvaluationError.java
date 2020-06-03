package errors;

import source.SourceSpan;

import static types.ErrorType.EVALUATE_ERROR;

public final class EvaluationError extends Error
{
    public EvaluationError(SourceSpan span, String errorMessage)
    {
        super(EVALUATE_ERROR, span, errorMessage);
    }

    public static String exceptionOccurred(Exception exception)
    {
        StackTraceElement stackTraceElement = exception.getStackTrace()[0];
        int lineNumber = stackTraceElement.getLineNumber();
        String className = stackTraceElement.getClassName();

        String location = String.format("Line %1s: Evaluation exception occurred at %2s.", lineNumber, className);
        String message = exception.getMessage();

        return location + "\n" + message;
    }

    public static String unexpectedExpression(String expression)
    {
        return String.format("Unexpected expression '%s'.", expression);
    }

    public static String unexpectedStatement(String statement)
    {
        return String.format("Unexpected statement '%s'.", statement);
    }

    public static String unexpectedUnaryObjectType(String type)
    {
        return String.format("Unexpected unary object type '%s'.", type);
    }

    public static String unexpectedBinaryObjectTypes(String leftType, String rightType)
    {
        return String.format("Unexpected binary object types '%1s' and '%2s'.", leftType, rightType);
    }

    public static String unexpectedAssignmentObjectTypes(String leftType, String rightType)
    {
        return String.format("Unexpected assignment object types '%1s' and '%2s'.", leftType, rightType);
    }

    public static String unexpectedUnaryOperator(String operator)
    {
        return String.format("Unexpected unary operator '%s'.", operator);
    }

    public static String unexpectedBinaryOperator(String operator)
    {
        return String.format("Unexpected binary operator '%s'.", operator);
    }

    public static String unexpectedAssignmentOperator(String operator)
    {
        return String.format("Unexpected assignment operator '%s'.", operator);
    }

    public static String missingSymbol(String name)
    {
        return String.format("Symbol '%s' does not exist in symbol table.", name);
    }

    public static String invalidLoopTypes(String lowerBound, String upperBound)
    {
        return String.format("Invalid loop object types '%1s' and '%2s'.", lowerBound, upperBound);
    }

    public static String invalidConditionalType(String type)
    {
        return String.format("Invalid conditional object type '%s'.", type);
    }
}
