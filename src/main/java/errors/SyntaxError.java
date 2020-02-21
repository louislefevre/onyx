package errors;

import lombok.Getter;
import identifiers.ErrorType;

final class SyntaxError extends Error
{
    @Getter private final String errorMessage;
    @Getter private final ErrorType errorType;

    public SyntaxError(String errorMessage)
    {
        this.errorMessage = errorMessage;
        this.errorType = ErrorType.SYNTAX_ERROR;
    }
}
