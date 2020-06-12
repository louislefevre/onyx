package errors;

import source.SourceSpan;

import static types.ErrorType.LEXICAL_ERROR;

/**
 * The LexicalError class is used to represent an error that occurred during the lexical analysis stage of compilation.
 * <p>
 * Static methods for generating LexicalErrors are stored here, each of which uses a set of pre-defined input Strings.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class LexicalError extends Error
{
    public LexicalError(SourceSpan span, String problem, String solution)
    {
        super(LEXICAL_ERROR, span, problem, solution);
    }

    /**
     * Generate and return a LexicalError for an invalid integer.
     *
     * @param syntax The syntax of the integer
     * @param start The start position of the syntax
     * @param length The length of the syntax
     * @return A LexicalError containing information about an error that occurred
     */
    public static LexicalError invalidInt(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String problem = String.format("The number '%s' is too large.", syntax);
        String solution = "Use a number less than 2,147,483,647.";
        return new LexicalError(span, problem, solution);
    }

    /**
     * Generate and return a LexicalError for an invalid double.
     *
     * @param syntax The syntax of the double
     * @param start The start position of the syntax
     * @param length The length of the syntax
     * @return A LexicalError containing information about an error that occurred
     */
    public static LexicalError invalidDouble(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String problem = String.format("The number '%s' is too large.", syntax);
        String solution = "Use a number with less than 15 decimal digits.";
        return new LexicalError(span, problem, solution);
    }

    /**
     * Generate and return a LexicalError for an invalid character.
     *
     * @param syntax The syntax of the character
     * @param start The start position of the syntax
     * @param length The length of the syntax
     * @return A LexicalError containing information about an error that occurred
     */
    public static LexicalError badCharacter(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String problem = String.format("The character '%s' is invalid and not recognised by the language.", syntax);
        String solution = "Try removing the character, or replace with it a valid one.";
        return new LexicalError(span, problem, solution);
    }

    /**
     * Generate and return a LexicalError for an incomplete string.
     *
     * @param syntax The syntax of the string
     * @param start The start position of the syntax
     * @param length The length of the syntax
     * @return A LexicalError containing information about an error that occurred
     */
    public static LexicalError incompleteString(String syntax, int start, int length)
    {
        SourceSpan span = new SourceSpan(start, length);
        String problem = String.format("The string '%s' is incomplete due to a missing closing quotation mark.", syntax);
        String solution = "Add a quotation mark (\") to the end of the string to close it.";
        return new LexicalError(span, problem, solution);
    }
}
