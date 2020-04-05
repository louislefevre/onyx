package errors;

import analysis.lexical.SyntaxSpan;
import identifiers.ErrorType;
import lombok.Getter;

@Getter
public abstract class Error
{
    private final SyntaxSpan span;
    private final String errorMessage;

    public Error(SyntaxSpan span, String errorMessage)
    {
        this.span = span;
        this.errorMessage = errorMessage;
    }

    public abstract ErrorType getErrorType();
}
