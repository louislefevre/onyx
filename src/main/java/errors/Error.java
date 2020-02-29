package errors;

import identifiers.ErrorType;
import lombok.Getter;

public abstract class Error
{
    @Getter private final TextSpan span;
    @Getter private final String errorMessage;

    public Error(TextSpan span, String errorMessage)
    {
        this.span = span;
        this.errorMessage = errorMessage;
    }

    public abstract ErrorType getErrorType();
}
