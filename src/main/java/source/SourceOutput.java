package source;

import compilation.generation.Evaluator;
import errors.ErrorHandler;
import exceptions.Exception;
import javafx.scene.Node;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;

public final class SourceOutput
{
    private final Evaluator evaluator;
    private final ErrorHandler errorHandler;
    private final TextFlow output;

    public SourceOutput(Evaluator evaluator, ErrorHandler errorHandler)
    {
        this.evaluator = evaluator;
        this.errorHandler = errorHandler;
        this.output = evaluateResult();
    }

    public TextFlow getOutput()
    {
        return output;
    }

    public String getRawOutput()
    {
        TextFlow textFlow = getOutput();
        StringBuilder builder = new StringBuilder();

        for (Node child : textFlow.getChildren())
            builder.append(((Text) child).getText());

        return builder.toString();
    }

    private TextFlow evaluateResult()
    {
        Object result;
        try
        {
            result = evaluator.getEvaluation();
            if (errorHandler.containsErrors())
                return errorHandler.getPrimaryError();
        }
        catch (Exception exception)
        {
            result = exception.getMessage();
        }

        Text text = new Text(result.toString());
        text.setFill(Color.WHITE);

        return new TextFlow(text);
    }
}
