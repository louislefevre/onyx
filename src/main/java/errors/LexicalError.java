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

    public static Error invalidInt(String syntax, int start, int length)
    {
        TextSpan span = new TextSpan(start, length);
        String message = String.format("ERROR: The number '%s' isn't a valid int.", syntax);
        return new LexicalError(span, message);
    }

    public static Error badCharacter(String syntax, int start, int length)
    {
        TextSpan span = new TextSpan(start, length);
        String message = String.format("ERROR: Bad character '%s'.", syntax);
        return new LexicalError(span, message);
    }
}
