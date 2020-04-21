package errors;

import source.SourceSpan;
import identifiers.ObjectType;
import lombok.Getter;
import identifiers.ErrorType;
import org.jetbrains.annotations.NotNull;

@Getter
public final class EvaluateError extends Error
{
    private final ErrorType errorType;

    public EvaluateError(SourceSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.EVALUATE_ERROR;
    }

    @NotNull
    public static EvaluateError unexpectedNode(ObjectType type)
    {
        String message = String.format("Unexpected node '%s'.", type);
        return new EvaluateError(null, message);
    }

    @NotNull
    public static EvaluateError unexpectedUnaryOperator(ObjectType type)
    {
        String message = String.format("Unexpected unary operator '%s'.", type);
        return new EvaluateError(null, message);
    }

    @NotNull
    public static EvaluateError unexpectedBinaryOperator(ObjectType type)
    {
        String message = String.format("Unexpected binary operator '%s'.", type);
        return new EvaluateError(null, message);
    }
}
