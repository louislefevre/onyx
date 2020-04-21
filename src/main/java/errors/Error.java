package errors;

import source.SourceSpan;
import identifiers.ErrorType;
import lombok.Getter;

@Getter
public abstract class Error
{
    private final SourceSpan span;
    private final String errorMessage;

    public Error(SourceSpan span, String errorMessage)
    {
        this.span = span;
        this.errorMessage = errorMessage;
    }

    public abstract ErrorType getErrorType();
}
