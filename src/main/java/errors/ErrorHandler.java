package errors;

import misc.ANSI;

import java.util.List;

public final class ErrorHandler
{
    private static List<Error> errorsLog;

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
        return true;
    }

    public static void addLexicalError(String message)
    {
        addError(new LexicalError(message));
    }

    private static void addError(Error error)
    {
        errorsLog.add(error);
    }

    private static void showErrors()
    {
        for (Error error : errorsLog)
            System.out.println(ANSI.RED + error + ANSI.RESET);
    }
}
