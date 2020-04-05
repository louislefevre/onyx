package synthesis.generation;

import errors.ErrorHandler;
import org.jetbrains.annotations.NotNull;

public final class SourceOutput
{
    private final Object getResult;
    private final boolean failed;

    public SourceOutput(@NotNull Evaluator evaluator, @NotNull ErrorHandler errorHandler)
    {
        this.getResult = evaluator.getEvaluation();
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
