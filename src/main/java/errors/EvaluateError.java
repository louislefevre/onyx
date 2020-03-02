package errors;

import identifiers.ObjectType;
import lombok.Getter;
import identifiers.ErrorType;

public final class EvaluateError extends Error
{
    @Getter private final ErrorType errorType;

    public EvaluateError(TextSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.EVALUATE_ERROR;
    }

    public static Error unexpectedNode(ObjectType type)
    {
        String message = String.format("Unexpected node '%s'.", type);
        return new LexicalError(null, message);
    }

    public static Error unexpectedUnaryOperator(Object type)
    {
        String message = String.format("Unexpected unary operator '%s'.", type);
        return new LexicalError(null, message);
    }

    public static Error unexpectedBinaryOperator(Object type)
    {
        String message = String.format("Unexpected binary operator '%s'.", type);
        return new LexicalError(null, message);
    }
}
