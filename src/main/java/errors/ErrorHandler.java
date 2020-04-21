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

    public boolean errorsPresent()
    {
        return !this.errorsLog.isEmpty();
    }

    public void outputErrors()
    {
        for (Error error : this.errorsLog)
        {
            int errorStart = error.getSpan().getStart();
            int errorEnd = error.getSpan().getEnd();
            int lineIndex = this.sourceInput.getLineIndex(errorStart);

            SourceLine line = this.sourceInput.getSourceLines().get(lineIndex);
            int lineStart = line.getStart();
            int lineEnd = line.getEnd();
            int character = errorStart - lineStart + 1;

            // For handling unexpected EOF_TOKEN errors; results in out of bounds exception otherwise
            String appendages = "";
            if (errorEnd > this.sourceInput.length())
            {
                errorEnd--;
                lineEnd++;
                appendages = "_";
            }

            String lineInfo = String.format(" (%1s,%2s): ", lineIndex + 1, character);
            String errorMessage = ANSI.RED +
                                  error.getErrorType() +
                                  lineInfo +
                                  error.getErrorMessage() +
                                  ANSI.RESET;

            String prefixSyntax = ANSI.GREY + this.sourceInput.substring(lineStart, errorStart) + ANSI.RESET;
            String errorSyntax = ANSI.RED + this.sourceInput.substring(errorStart, errorEnd) + appendages + ANSI.RESET;
            String suffixSyntax = ANSI.GREY + this.sourceInput.substring(errorEnd, lineEnd) + ANSI.RESET;
            String fullSyntax = "    " + prefixSyntax + errorSyntax + suffixSyntax;

            System.out.println(errorMessage);
            System.out.println(fullSyntax);
        }
    }
}
