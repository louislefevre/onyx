package errors;

import source.SourceInput;
import source.SourceLine;
import util.ANSI;

import java.util.ArrayList;
import java.util.List;

public final class ErrorHandler
{
    private final List<Error> errorsLog;
    private final SourceInput sourceInput;

    public ErrorHandler(SourceInput sourceInput)
    {
        this.errorsLog = new ArrayList<>();
        this.sourceInput = sourceInput;
    }

    public void addError(Error error)
    {
        this.errorsLog.add(error);
    }

    public boolean containsErrors()
    {
        return !this.errorsLog.isEmpty();
    }

    public String getErrors()
    {
        StringBuilder builder = new StringBuilder();
        for (Error error : this.errorsLog)
        {
            int errorStart = error.getSpan().getStart();
            int errorEnd = error.getSpan().getEnd();
            int lineIndex = this.sourceInput.getLineIndex(errorStart);

            SourceLine line = this.sourceInput.getSourceLines().get(lineIndex);
            int lineStart = line.getStart();
            int lineEnd = line.getEnd();
            int character = errorStart - lineStart + 1;

            String lineInfo = String.format(" (%1s,%2s): ", lineIndex + 1, character);
            String errorMessage = ANSI.RED + error.toString() + lineInfo + error.getErrorMessage();

            String prefixSyntax, errorSyntax, suffixSyntax, fullSyntax;
            if (errorEnd > this.sourceInput.length()) // Handles unexpected EOF_TOKEN errors
            {
                prefixSyntax = ANSI.GREY + this.sourceInput.substring(lineStart, lineEnd);
                errorSyntax = ANSI.RED + "_";
                suffixSyntax = ANSI.GREY + "";
            }
            else // Handles all other errors
            {
                prefixSyntax = ANSI.GREY + this.sourceInput.substring(lineStart, errorStart);
                errorSyntax = ANSI.RED + this.sourceInput.substring(errorStart, errorEnd);
                suffixSyntax = ANSI.GREY + this.sourceInput.substring(errorEnd, lineEnd);
            }
            fullSyntax = "    " + prefixSyntax + errorSyntax + suffixSyntax + ANSI.RESET;

            builder.append(errorMessage);
            builder.append(fullSyntax);
        }
        return builder.toString();
    }
}
