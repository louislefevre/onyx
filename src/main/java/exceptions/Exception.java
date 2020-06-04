package exceptions;

import types.ExceptionType;

public abstract class Exception extends Throwable
{
    private final ExceptionType exceptionType;
    private final String message;

    public Exception(ExceptionType exceptionType, String message)
    {
        this.exceptionType = exceptionType;
        this.message = message;
    }

    @Override
    public String getMessage()
    {
        StackTraceElement stackTraceElement = getStackTrace()[0];
        int lineNumber = stackTraceElement.getLineNumber();
        String className = stackTraceElement.getClassName();

        StringBuilder builder = new StringBuilder();
        builder.append(String.format("%1s in %2s (line %3s):", exceptionType, className, lineNumber));
        builder.append(System.lineSeparator());
        builder.append(message);

        return builder.toString();
    }
}
