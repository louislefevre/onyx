package errors;

import analysis.lexical.SyntaxSpan;
import lombok.Getter;
import identifiers.ErrorType;
import org.jetbrains.annotations.NotNull;

@Getter
public final class LexicalError extends Error
{
    private final ErrorType errorType;

    public LexicalError(SyntaxSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.LEXICAL_ERROR;
    }

    @NotNull
    public static LexicalError invalidInt(String syntax, int start, int length)
    {
        SyntaxSpan span = new SyntaxSpan(start, length);
        String message = String.format("The number '%s' isn't a valid int.", syntax);
        return new LexicalError(span, message);
    }

    @NotNull
    public static LexicalError badCharacter(String syntax, int start, int length)
    {
        SyntaxSpan span = new SyntaxSpan(start, length);
        String message = String.format("Bad character '%s'.", syntax);
        return new LexicalError(span, message);
    }
}
