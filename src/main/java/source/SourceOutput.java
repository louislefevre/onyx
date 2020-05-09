package source;

import errors.ErrorHandler;
import javafx.scene.text.Text;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

import java.util.List;

public final class SourceOutput
{
    private final Object result;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;
    private final boolean replMode;
    private final SourceDisplay sourceDisplay;

    public SourceOutput(Evaluator evaluator)
    {
        this.result = evaluator.getEvaluation();
        this.errorHandler = evaluator.getErrorHandler();
        this.symbolTable = evaluator.getSymbolTable();
        this.replMode = evaluator.isReplMode();
        this.sourceDisplay = new SourceDisplay(result, errorHandler.getSourceInput(), errorHandler);
    }

    public Object getOutput()
    {
        if (errorHandler.containsErrors())
            return sourceDisplay.getErrors();

        return result;
    }

    public Object getDecoratedOutput()
    {
        if (errorHandler.containsErrors())
            return sourceDisplay.getDecoratedErrors();

        return result;
    }

    public List<Text> getTextOutput()
    {
        if (errorHandler.containsErrors())
            return sourceDisplay.getTextErrors();

        return sourceDisplay.getTextResult();
    }
}
