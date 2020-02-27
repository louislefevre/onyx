package errors;

import identifiers.ErrorType;
import lombok.Getter;

public final class EvaluateError extends Error
{
    @Getter private final String errorMessage;
    @Getter private final ErrorType errorType;

    public EvaluateError(String errorMessage)
    {
        this.errorMessage = errorMessage;
        this.errorType = ErrorType.EVALUATE_ERROR;
    }
}
