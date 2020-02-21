package errors;

public final class LexicalError extends Error
{
    private final String errorMessage;

    public LexicalError(String errorMessage)
    {
        this.errorMessage = errorMessage;
    }

    @Override
    public String getErrorMessage()
    {
        return this.errorMessage;
    }
}
