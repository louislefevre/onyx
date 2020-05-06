package source;

import errors.ErrorHandler;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

import java.util.ArrayList;
import java.util.List;

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

    public Object getResult()
    {
        if (errorHandler.containsErrors())
            return errorHandler.getErrors();

        return result;
    }

    public List<Text> getTextResult()
    {
        if (this.errorHandler.containsErrors())
            return this.errorHandler.getTextErrors();

        return stringToText(this.result.toString());
    }

    private static List<Text> stringToText(String str)
    {
        List<Text> textList = new ArrayList<>();
        Text text = new Text(str);
        text.setFill(Color.WHITE);
        textList.add(text);
        return textList;
    }
}
