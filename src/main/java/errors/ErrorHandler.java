package errors;

import java.util.List;

public final class ErrorHandler
{
    private static List<Error> errorsLog;

    private ErrorHandler()
    {
        // Prevents class instantiation
        throw new UnsupportedOperationException();
    }

    public static List<Error> getErrorsLog()
    {
        return errorsLog;
    }

    public static void addError(Error error)
    {
        errorsLog.add(error);
    }
}
