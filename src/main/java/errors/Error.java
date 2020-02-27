package errors;

import identifiers.ErrorType;

public abstract class Error
{
    public abstract ErrorType getErrorType();
    public abstract String getErrorMessage();
}
