package errors;

import symbols.ErrorType;

abstract class Error
{
    public abstract ErrorType getErrorType();
    public abstract String getErrorMessage();
}
