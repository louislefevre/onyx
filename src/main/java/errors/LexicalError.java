package errors;

import lombok.Getter;
import identifiers.ErrorType;

public final class LexicalError extends Error
{
    @Getter private final ErrorType errorType;

    public LexicalError(TextSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.LEXICAL_ERROR;
    }

    public static Error invalidNumber(String syntax, Object type, int start, int length)
    {
        TextSpan span = new TextSpan(start, length);
        String message = String.format("The number '%1s' isn't a valid %2s", syntax, type);
        return new LexicalError(span, message);
    }

    public static Error badToken(String syntax, Object type, int start, int length)
    {
        TextSpan span = new TextSpan(start, length);
        String message = String.format("ERROR: Bad character '%1s' of type %2s", syntax, type);
        return new LexicalError(span, message);
    }
}
