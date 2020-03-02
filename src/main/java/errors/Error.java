package errors;

import analysis.lexical.SyntaxSpan;
import identifiers.ErrorType;
import lombok.Getter;

public abstract class Error
{
    @Getter private final SyntaxSpan span;
    @Getter private final String errorMessage;

    public Error(SyntaxSpan span, String errorMessage)
    {
        this.span = span;
        this.errorMessage = errorMessage;
    }

    public abstract ErrorType getErrorType();
}
