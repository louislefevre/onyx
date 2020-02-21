package errors;

import identifiers.ErrorType;

abstract class Error
{
    public abstract ErrorType getErrorType();
    public abstract String getErrorMessage();
}
