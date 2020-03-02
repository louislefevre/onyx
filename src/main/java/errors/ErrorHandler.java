package errors;

import synthesis.generation.Evaluator;
import util.ANSI;

import java.util.List;

public final class ErrorHandler
{
    private final Object evaluation;
    private final List<Error> errorsLog;
    private final String input;

    public ErrorHandler(Evaluator evaluator, String input)
    {
        this.evaluation = evaluator.evaluate();
        this.errorsLog = evaluator.getErrorLog();
        this.input = input;
    }

    public Object getEvaluation()
    {
        if(!this.errorsPresent())
            return this.evaluation;

        this.printErrors();
        return null;
    }

    public boolean errorsPresent()
    {
        return !this.errorsLog.isEmpty();
    }

    private void printErrors()
    {
        for (Error error : this.errorsLog)
        {
            int start = error.getSpan().getStart();
            int length = error.getSpan().getLength();
            int end = error.getSpan().getEnd();

            String prefixSyntax = ANSI.GREY + this.input.substring(0, start) + ANSI.RESET;
            String errorSyntax = ANSI.RED + this.input.substring(start, start+length) + ANSI.RESET;
            String suffixSyntax = ANSI.GREY + this.input.substring(end) + ANSI.RESET;

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
