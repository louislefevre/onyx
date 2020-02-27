package errors;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import synthesis.generation.Evaluator;
import util.ANSI;

import java.util.ArrayList;
import java.util.List;

public final class ErrorHandler
{
    private final List<Error> errorsLog;

    public ErrorHandler(Lexer lexer, Parser parser, TypeChecker typeChecker, Evaluator evaluator)
    {
        this.errorsLog = new ArrayList<>();
        this.retrieveErrors(lexer.getErrorLog(),
                            parser.getErrorLog(),
                            typeChecker.getErrorLog(),
                            evaluator.getErrorLog());
    }

    public boolean errorsPresent()
    {
        if(this.errorsLog.isEmpty())
            return false;

        this.printErrors();
        return true;
    }

    @SafeVarargs
    private void retrieveErrors(List<Error> ... errorLogsList)
    {
        for(List<Error> errorLog : errorLogsList)
            this.errorsLog.addAll(errorLog);
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
