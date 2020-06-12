package errors;

import compilation.analysis.lexical.Token;
import source.SourceSpan;
import types.TokenType;

import static types.ErrorType.SYNTAX_ERROR;

/**
 * The SyntaxError class is used to represent an error that occurred during the syntax analysis stage of compilation.
 * <p>
 * Static methods for generating SyntaxErrors are stored here, each of which uses a set of pre-defined input Strings.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class SyntaxError extends Error
{
    public SyntaxError(SourceSpan span, String problem, String solution)
    {
        super(SYNTAX_ERROR, span, problem, solution);
    }

    /**
     * Generate and return a SyntaxError for an incomplete expression.
     *
     * @param span The location information for where an error occurred
     * @return A SyntaxError containing information about an error that occurred
     */
    public static SyntaxError incompleteExpression(SourceSpan span)
    {
        String problem = "This line is incomplete and ends unexpectedly.";
        String solution = "Refer to the wiki and read the section related to what you are trying to do.";
        return new SyntaxError(span, problem, solution);
    }

    /**
     * Generate and return a SyntaxError for an unexpected token.
     *
     * @param token The Token which caused the error
     * @param expectedType The TokenType that was expected
     * @return A SyntaxError containing information about an error that occurred
     */
    public static SyntaxError unexpectedToken(Token token, TokenType expectedType)
    {
        SourceSpan span = token.getSpan();
        String syntax = token.getSyntax();

        String problem, solution;
        switch (expectedType)
        {
            case LINE_BREAK_TOKEN:
                problem = "The end of the line should be here.";
                solution = String.format("Move the text '%s' and beyond down to the next line.", syntax);
                break;
            case CLOSE_PARENTHESIS_TOKEN:
                problem = "A close parenthesis character should be here.";
                solution = "Add a close parenthesis character (\")\") here, or remove the opening parenthesis character.";
                break;
            case TO_TOKEN:
                problem = "The \"to\" keyword is missing from the loop statement.";
                solution = "Add the \"to\" keyword where the error is marked.";
                break;
            case CLOSE_BRACE_TOKEN:
                problem = "A close brace character should be here.";
                solution = "Add a close brace character (\"}\") here, or remove the opening brace character.";
                break;
            default:
                return incompleteExpression(span);
        }

        return new SyntaxError(span, problem, solution);
    }

    /**
     * Generate and return a SyntaxError for an invalid statement.
     *
     * @param span The location information for where an error occurred
     * @return A SyntaxError containing information about an error that occurred
     */
    public static SyntaxError invalidStatement(SourceSpan span)
    {
        String problem = "Expressions cannot be written on their own.";
        String solution = "Try assigning this expression to a variable first. " + System.lineSeparator() +
                          "If you are trying to print a variable, begin the expression with the variable name (e.g. \"var + 5\").";
        return new SyntaxError(span, problem, solution);
    }

    /**
     * Generate and return a SyntaxError for a set of empty parenthesis.
     *
     * @param openParenSpan The location information for the open parenthesis token
     * @param closeParenSpan The location information for the close parenthesis token
     * @return A SyntaxError containing information about an error that occurred
     */
    public static SyntaxError emptyParenthesis(SourceSpan openParenSpan, SourceSpan closeParenSpan)
    {
        SourceSpan span = SourceSpan.inRange(openParenSpan.getStart(), closeParenSpan.getEnd());
        String problem = "Parenthesis cannot be empty.";
        String solution = "Write an expression within the parenthesis, or remove them entirely.";
        return new SyntaxError(span, problem, solution);
    }
}
