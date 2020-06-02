package errors;

import source.SourceSpan;

import static identifiers.ErrorType.LEXICAL_ERROR;

public final class LexicalError extends Error
{
    public LexicalError(SourceSpan span, String errorMessage)
    {
        super(span, LEXICAL_ERROR, errorMessage);
    }

    public static LexicalError invalidInt(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String message = String.format("The number '%s' isn't a valid int.", syntax);
        return new LexicalError(span, message);
    }

    public static LexicalError invalidDouble(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String message = String.format("The number '%s' isn't a valid double.", syntax);
        return new LexicalError(span, message);
    }

    public static LexicalError badCharacter(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String message = String.format("Bad character '%s'.", syntax);
        return new LexicalError(span, message);
    }

    public static LexicalError incompleteString(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String message = String.format("Incomplete string '%s'.", syntax);
        return new LexicalError(span, message);
    }
}
