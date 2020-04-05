package errors;

import analysis.lexical.SyntaxSpan;
import identifiers.ObjectType;
import lombok.Getter;
import identifiers.ErrorType;

@Getter
public final class EvaluateError extends Error
{
    private final ErrorType errorType;

    public EvaluateError(SyntaxSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.EVALUATE_ERROR;
    }

    public static EvaluateError unexpectedNode(ObjectType type)
    {
        String message = String.format("Unexpected node '%s'.", type);
        return new EvaluateError(null, message);
    }

    public static EvaluateError unexpectedUnaryOperator(ObjectType type)
    {
        String message = String.format("Unexpected unary operator '%s'.", type);
        return new EvaluateError(null, message);
    }

    public static EvaluateError unexpectedBinaryOperator(ObjectType type)
    {
        String message = String.format("Unexpected binary operator '%s'.", type);
        return new EvaluateError(null, message);
    }
}
