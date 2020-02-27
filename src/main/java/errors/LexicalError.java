package errors;

import lombok.Getter;
import identifiers.ErrorType;

public final class LexicalError extends Error
{
    @Getter private final String errorMessage;
    @Getter private final ErrorType errorType;

    public LexicalError(String errorMessage)
    {
        this.errorMessage = errorMessage;
        this.errorType = ErrorType.LEXICAL_ERROR;
    }
}
