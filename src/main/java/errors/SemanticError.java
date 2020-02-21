package errors;

public final class SemanticError extends Error
{
    private final String errorMessage;

    public SemanticError(String errorMessage)
    {
        this.errorMessage = errorMessage;
    }

    @Override
    public String getErrorMessage()
    {
        return this.errorMessage;
    }
}
