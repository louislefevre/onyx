package errors;

import source.SourceSpan;

import static types.ErrorType.LEXICAL_ERROR;

public final class LexicalError extends Error
{
    public LexicalError(SourceSpan span, String problem, String solution)
    {
        super(LEXICAL_ERROR, span, problem, solution);
    }

    public static LexicalError invalidInt(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String problem = String.format("The number '%s' is too large.", syntax);
        String solution = "Use a number less than 2,147,483,647.";
        return new LexicalError(span, problem, solution);
    }

    public static LexicalError invalidDouble(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String problem = String.format("The number '%s' is too large.", syntax);
        String solution = "Use a number with less than 15 decimal digits.";
        return new LexicalError(span, problem, solution);
    }

    public static LexicalError badCharacter(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String problem = String.format("The character '%s' is invalid and not recognised by the language.", syntax);
        String solution = "Try removing the character, or replace with it a valid one.";
        return new LexicalError(span, problem, solution);
    }

    public static LexicalError incompleteString(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String problem = String.format("The string '%s' is incomplete due to a missing closing quotation mark.", syntax);
        String solution = "Add a quotation mark (\") to the end of the string to close it.";
        return new LexicalError(span, problem, solution);
    }
}
