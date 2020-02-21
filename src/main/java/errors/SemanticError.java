package errors;

import lombok.Getter;
import identifiers.ErrorType;

final class SemanticError extends Error
{
    @Getter private final String errorMessage;
    @Getter private final ErrorType errorType;

    public SemanticError(String errorMessage)
    {
        this.errorMessage = errorMessage;
        this.errorType = ErrorType.SEMANTIC_ERROR;
    }
}
