package source;

import errors.ErrorHandler;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

public final class SourceOutput
{
    private final Object result;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;

    public SourceOutput(Evaluator evaluator)
    {
        this.result = evaluator.getEvaluation();
        this.errorHandler = evaluator.getErrorHandler();
        this.symbolTable = evaluator.getSymbolTable();
    }

    public Object getResult()
    {
        if(errorHandler.containsErrors())
            return errorHandler.getErrors();

        return result;
    }
}
