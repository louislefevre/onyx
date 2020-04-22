package errors;

import identifiers.ErrorType;
import lombok.Getter;
import source.SourceSpan;

@Getter
public final class EvaluateError extends Error
{
    private final ErrorType errorType;

    public EvaluateError(SourceSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.EVALUATE_ERROR;
    }

    public static Exception unexpectedExpression(String expression)
    {
        String message = String.format("Unexpected expression '%s'.", expression);
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
