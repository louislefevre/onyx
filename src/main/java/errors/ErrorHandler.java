package errors;

import synthesis.generation.Evaluator;
import util.ANSI;

import java.util.List;

public final class ErrorHandler
{
    private final Object evaluation;
    private final List<Error> errorsLog;

    public ErrorHandler(Evaluator evaluator)
    {
        this.evaluation = evaluator.evaluate();
        this.errorsLog = evaluator.getErrorLog();
    }

    public Object getEvaluation()
    {
        if(!this.errorsPresent())
            return this.evaluation;

        this.printErrors();
        return null;
    }

    private boolean errorsPresent()
    {
        return !this.errorsLog.isEmpty();
    }

    private void printErrors()
    {
        for (Error error : this.errorsLog)
        {
            System.out.print(ANSI.RED);
            System.out.println(error.getErrorType());
            System.out.println(error.getErrorMessage());
            System.out.print(ANSI.RESET);
        }
    }
}
