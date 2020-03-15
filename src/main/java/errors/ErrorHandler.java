package errors;

import util.ANSI;

import java.util.ArrayList;
import java.util.List;

public final class ErrorHandler
{
    private final List<Error> errorsLog;
    private final String input;

    public ErrorHandler(String input)
    {
        this.errorsLog = new ArrayList<>();
        this.input = input;
    }

    public boolean errorsPresent()
    {
        if(this.errorsLog.isEmpty())
            return false;
        this.printErrors();
        return true;
    }

    public void addError(Error error)
    {
        this.errorsLog.add(error);
    }

    private void printErrors()
    {
        for (Error error : this.errorsLog)
        {
            String input = this.input;
            int start = error.getSpan().getStart();
            int end = error.getSpan().getEnd();

            // For handling unexpected EOF_TOKEN errors; results in out of bounds exception otherwise
            if(end > this.input.length())
                input += "_";

            String prefixSyntax = ANSI.GREY + input.substring(0, start) + ANSI.RESET;
            String errorSyntax = ANSI.RED + input.substring(start, end) + ANSI.RESET;
            String suffixSyntax = ANSI.GREY + input.substring(end) + ANSI.RESET;

            String fullSyntax = prefixSyntax + errorSyntax + suffixSyntax;

            String errorMessage = ANSI.RED +
                                  error.getErrorType() +
                                  ": " +
                                  error.getErrorMessage() +
                                  ANSI.RESET;

            System.out.println(errorMessage);
            System.out.println(fullSyntax);
        }
    }
}
