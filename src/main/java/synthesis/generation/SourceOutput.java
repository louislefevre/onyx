package synthesis.generation;

import errors.ErrorHandler;

public final class SourceOutput
{
    private final Object output;

    public SourceOutput(ErrorHandler errorHandler)
    {
        this.output = errorHandler.getEvaluation();
    }

    public Object getOutput()
    {
        return output;
    }
}
