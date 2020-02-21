package errors;

import misc.ANSI;

import java.util.ArrayList;
import java.util.List;

public final class ErrorHandler
{
    private final static List<Error> errorsLog = new ArrayList<>();

    private ErrorHandler()
    {
        // Prevents class instantiation
        throw new UnsupportedOperationException();
    }

    public static boolean errorsPresent()
    {
        if(errorsLog.isEmpty())
            return false;
        showErrors();
        errorsLog.clear();
        return true;
    }

    public static void addLexicalError(String message)
    {
        addError(new LexicalError(message));
    }

    public static void addSyntaxError(String message)
    {
        addError(new SyntaxError(message));
    }

    public static void addSemanticError(String message)
    {
        addError(new SemanticError(message));
    }

    private static void addError(Error error)
    {
        errorsLog.add(error);
    }

    private static void showErrors()
    {
        for (Error error : errorsLog)
            System.out.println(ANSI.RED + error.getErrorMessage() + ANSI.RESET);
    }
}
