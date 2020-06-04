package errors;

import lombok.Getter;
import source.SourceInput;
import source.SourceLine;
import source.SourceSpan;
import types.ErrorType;

@Getter
public abstract class Error
{
    private final SourceSpan span;
    private final ErrorType errorType;
    private final String message;

    public Error(ErrorType errorType, SourceSpan span, String message)
    {
        this.span = span;
        this.errorType = errorType;
        this.message = message;
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

        private ErrorMessage(Error error, SourceInput sourceInput)
        {
            initialiseErrorMessage(error, sourceInput);
        }

        private void initialiseErrorMessage(Error error, SourceInput sourceInput)
        {
            int errorStart = error.getSpan().getStart();
            int errorEnd = error.getSpan().getEnd();
            int lineIndex = sourceInput.getLineIndex(errorStart);

            SourceLine line = sourceInput.getSourceLines().get(lineIndex);
            int lineStart = line.getStart();
            int lineEnd = line.getEnd();
            int character = errorStart - lineStart + 1;

            String lineBreak = System.lineSeparator();
            String commentRegex = "#(?=([^\"]*\"[^\"]*\")*[^\"]*$)"; // Removes text occurring after a hash, but excludes hashes within quotes.
            String lineInfo = String.format(" (%1s,%2s): ", lineIndex + 1, character);
            String errorInfo = error.getErrorType() + lineInfo + error.getMessage();
            String prefixSyntax, errorSyntax, suffixSyntax;

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
            suffixSyntax = suffixSyntax.stripTrailing();

            errorInfo += lineBreak;
            suffixSyntax += lineBreak;

            this.errorInfo = errorInfo;
            this.prefixSyntax = "    " + prefixSyntax;
            this.errorSyntax = errorSyntax;
            this.suffixSyntax = suffixSyntax;
        }

        @Override
        public String toString()
        {
            return errorInfo + prefixSyntax + errorSyntax + suffixSyntax;
        }
    }
}
