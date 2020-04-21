package source;

import errors.ErrorHandler;
import synthesis.generation.Evaluator;

public final class SourceOutput
{
    private final Object result;
    private final ErrorHandler errorHandler;

    public SourceOutput(Evaluator evaluator, ErrorHandler errorHandler)
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
