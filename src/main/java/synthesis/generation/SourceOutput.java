package synthesis.generation;

import errors.ErrorHandler;

public final class SourceOutput
{
    private final Object getResult;
    private final boolean failed;

    public SourceOutput(ErrorHandler errorHandler)
    {
        this.getResult = errorHandler.getEvaluation();
        this.failed = errorHandler.errorsPresent();
    }

    public boolean compilationFailed()
    {
        return this.failed;
    }

    public Object getResult()
    {
        return getResult;
    }
}
