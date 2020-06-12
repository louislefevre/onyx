package errors;

import lombok.Getter;
import source.SourceInput;
import source.SourceLine;
import source.SourceSpan;
import types.ErrorType;

/**
 * The Error class is an abstract class used to represent errors that occur during compilation.
 * <p>
 * Children classes of Error call the parent constructor when initialised, and the majority of object generation is
 * performed here, which stores information about the error that occurred.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public abstract class Error
{
    private final ErrorType errorType;
    private final SourceSpan span;
    private final String problem;
    private final String solution;

    /**
     * Constructs an Error object for storing information about an error that occurred during compilation.
     * <p>
     * To generate an Error, it is required to provide a description of the problem and a solution. These must be
     * clear, detailed, and jargon-free. The type of error and location in the text must also be provided.
     *
     * @param errorType The type of error, which is based on where in the compilation process it occurred
     * @param span The location in the text that the error occurred
     * @param problem A description of the problem which caused the error
     * @param solution A description of a possible solution to solve the error
     */
    public Error(ErrorType errorType, SourceSpan span, String problem, String solution)
    {
        this.errorType = errorType;
        this.span = span;
        this.problem = problem;
        this.solution = solution;
    }

    /**
     * Returns an ErrorMessage object, containing formatted information about the error that occurred.
     * <p>
     * The same SourceInput object used during compilation is required in order to identify and visualised where
     * in the text the error occurred.
     *
     * @param sourceInput The original SourceInput used during compilation
     * @return An ErrorMessage object containing information about the error
     */
    public ErrorMessage getErrorMessage(SourceInput sourceInput)
    {
        return new ErrorMessage(this, sourceInput);
    }

    /**
     * The ErrorMessage class is used to store information about an error.
     * <p>
     * Extracts information from an Error object, retrieving data regarding the problem, solution, and location of
     * the error. This information is then formatted and split into various String objects, each of which hold a
     * particular portion of the overall error message.
     *
     * @author Louis Lefevre
     * @version 1.0
     * @since 1.0
     */
    @Getter
    public static class ErrorMessage
    {
        private String errorInfo;
        private String prefixSyntax;
        private String errorSyntax;
        private String suffixSyntax;
        private String problem;
        private String solution;

        private ErrorMessage(Error error, SourceInput sourceInput)
        {
            initialiseErrorMessage(error, sourceInput);
        }

        private void initialiseErrorMessage(Error error, SourceInput sourceInput)
        {
            // Error line info
            int errorStart = error.getSpan().getStart();
            int errorEnd = error.getSpan().getEnd();
            int lineIndex = sourceInput.getLineIndex(errorStart);

            // Source input line info
            SourceLine line = sourceInput.getSourceLines().get(lineIndex);
            int lineStart = line.getStart();
            int lineEnd = line.getEnd();
            int character = errorStart - lineStart + 1;

            // Miscellaneous strings
            String lineBreak = System.lineSeparator();
            String commentRegex = "#(?=([^\"]*\"[^\"]*\")*[^\"]*$)"; // Removes text occurring after a hash, but excludes hashes within quotes.

            // Error info
            errorInfo = String.format("Error occurred on line %s at character %s", lineIndex + 1, character);
            errorInfo += lineBreak;

            // Error printout
            if (errorEnd > sourceInput.length() || lineEnd < errorEnd) // Handles unexpected EOF_TOKEN and LINE_BREAK_TOKEN errors
            {
                prefixSyntax = sourceInput.substring(lineStart, lineEnd);
                errorSyntax = "_";
                suffixSyntax = "";
                prefixSyntax = prefixSyntax.split(commentRegex)[0];
            }
            else // Handles all other errors
            {
                prefixSyntax = sourceInput.substring(lineStart, errorStart);
                errorSyntax = sourceInput.substring(errorStart, errorEnd);
                suffixSyntax = sourceInput.substring(errorEnd, lineEnd);
                suffixSyntax = suffixSyntax.split(commentRegex)[0];
            }

            prefixSyntax = prefixSyntax.stripLeading();
            prefixSyntax = "    " + prefixSyntax;
            suffixSyntax = suffixSyntax.stripTrailing();
            suffixSyntax += lineBreak;

            // Error description
            problem = lineBreak + "Problem:" + lineBreak + error.getProblem() + lineBreak;
            solution = lineBreak + "Solution:" + lineBreak + error.getSolution() + lineBreak;
        }

        /**
         * Concatenates each of the Strings held by this object, and returns them as a single formatted String.
         *
         * @return A String containing the entire error message
         */
        @Override
        public String toString()
        {
            return errorInfo + prefixSyntax + errorSyntax + suffixSyntax + problem + solution;
        }
    }
}
