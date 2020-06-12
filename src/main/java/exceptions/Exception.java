package exceptions;

import types.ExceptionType;

/**
 * The Exception class is an abstract class used to represent exceptions that occur during compilation.
 * <p>
 * Children classes of Exception call the parent constructor when initialised, and the majority of object generation
 * is performed here, which stores information about the exception that occurred.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public abstract class Exception extends Throwable
{
    private final ExceptionType exceptionType;
    private final String message;

    /**
     * Constructs an Exception object for storing information about an exception that occurred during compilation.
     * <p>
     * To generate an Exception, the type of exception and a message detailing how it transpired must be defined.
     *
     * @param exceptionType The type of exception, which is based on where in the compilation process it occurred
     * @param message The message detailing what caused the exception to occur
     */
    public Exception(ExceptionType exceptionType, String message)
    {
        this.exceptionType = exceptionType;
        this.message = message;
    }

    /**
     * Return the message for this Exception object.
     * <p>
     * This message contains information about the location the Exception occurred in the code, the ExceptionType,
     * and the class in which it developed.
     *
     * @return A String containing information about the exception
     */
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
