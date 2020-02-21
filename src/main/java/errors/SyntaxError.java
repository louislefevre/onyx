package errors;

public final class SyntaxError extends Error
{
    private final String errorMessage;

    public SyntaxError(String errorMessage)
    {
        this.errorMessage = errorMessage;
    }

    @Override
    public String getErrorMessage()
    {
        return this.errorMessage;
    }
}
