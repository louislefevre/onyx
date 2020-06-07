package errors;

import lombok.Getter;
import source.SourceInput;
import source.SourceLine;
import source.SourceSpan;
import types.ErrorType;

@Getter
public abstract class Error
{
    private final ErrorType errorType;
    private final SourceSpan span;
    private final String problem;
    private final String solution;

    public Error(ErrorType errorType, SourceSpan span, String problem, String solution)
    {
        this.errorType = errorType;
        this.span = span;
        this.problem = problem;
        this.solution = solution;
    }

    public ErrorMessage getErrorMessage(SourceInput sourceInput)
    {
        return new ErrorMessage(this, sourceInput);
    }

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

        @Override
        public String toString()
        {
            return errorInfo + prefixSyntax + errorSyntax + suffixSyntax + problem + solution;
        }
    }
}
