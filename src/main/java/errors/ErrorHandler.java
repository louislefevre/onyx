package errors;

import source.SourceInput;
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
            int start = error.getSpan().getStart();
            int end = error.getSpan().getEnd();

            int lineIndex = this.sourceInput.getLineIndex(start);
            int character = start - this.sourceInput.getSourceLines().get(lineIndex).getStart() + 1;

            // For handling unexpected EOF_TOKEN errors; results in out of bounds exception otherwise
            String appendages = "";
            if (end > this.sourceInput.length())
            {
                end -= 1;
                appendages = "_";
            }

            String lineInfo = String.format(" (%1s,%2s): ", lineIndex + 1, character);
            String errorMessage = ANSI.RED +
                                  error.getErrorType() +
                                  lineInfo +
                                  error.getErrorMessage() +
                                  ANSI.RESET;

            String prefixSyntax = ANSI.GREY + this.sourceInput.substring(0, start) + ANSI.RESET;
            String errorSyntax = ANSI.RED + this.sourceInput.substring(start, end) + appendages + ANSI.RESET;
            String suffixSyntax = ANSI.GREY + this.sourceInput.substring(end) + ANSI.RESET;
            String fullSyntax = prefixSyntax + errorSyntax + suffixSyntax;

            System.out.println(errorMessage);
            System.out.println(fullSyntax);
        }
    }
}
