package source;

import errors.ErrorHandler;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

public final class SourceOutput
{
    private final Object result;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;
    private final boolean replMode;

    public SourceOutput(Evaluator evaluator)
    {
        this.result = evaluator.getEvaluation();
        this.errorHandler = evaluator.getErrorHandler();
        this.symbolTable = evaluator.getSymbolTable();
        this.replMode = evaluator.isReplMode();
    }

    public Object getOutput()
    {
        if (errorHandler.containsErrors())
            return errorHandler.getErrors();

        return result.toString();
    }

    public TextFlow getTextOutput()
    {
        if (errorHandler.containsErrors())
            return errorHandler.getPrimaryError();

        Text text = new Text(result.toString());
        text.setFill(Color.WHITE);

        return new TextFlow(text);
    }
}
