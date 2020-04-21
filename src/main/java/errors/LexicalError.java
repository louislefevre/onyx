package errors;

import identifiers.ErrorType;
import lombok.Getter;
import source.SourceSpan;

@Getter
public final class LexicalError extends Error
{
    private final ErrorType errorType;

    public LexicalError(SourceSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.LEXICAL_ERROR;
    }

    public static LexicalError invalidInt(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String message = String.format("The number '%s' isn't a valid int.", syntax);
        return new LexicalError(span, message);
    }

    public static LexicalError badCharacter(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String message = String.format("Bad character '%s'.", syntax);
        return new LexicalError(span, message);
    }
}
