package errors;

import identifiers.ErrorType;
import lombok.Getter;
import source.SourceSpan;

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

    @Override
    public abstract String toString();
}
