package synthesis.generation;

import errors.ErrorHandler;
import org.jetbrains.annotations.NotNull;

public final class SourceOutput
{
    private final Object result;
    private final ErrorHandler errorHandler;

    public SourceOutput(@NotNull Evaluator evaluator, @NotNull ErrorHandler errorHandler)
    {
        this.result = evaluator.getEvaluation();
        this.errorHandler = errorHandler;
    }

    public Object getResult()
    {
        if(this.errorHandler.errorsPresent())
        {
            this.errorHandler.outputErrors();
            return '\0';
        }

        return this.result;
    }
}
