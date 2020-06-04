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
    private final TextFlow output;

    public SourceOutput(Evaluator evaluator, ErrorHandler errorHandler)
    {
        this.output = evaluateResult(evaluator, errorHandler);
    }

    public TextFlow getOutput()
    {
        return output;
    }

    public String getRawOutput()
    {
        StringBuilder builder = new StringBuilder();
        for (Node child : output.getChildren())
            builder.append(((Text) child).getText());

        return builder.toString();
    }

    private static TextFlow evaluateResult(Evaluator evaluator, ErrorHandler errorHandler)
    {
        String result;
        try
        {
            Object[] outputArray = evaluator.getEvaluation();
            result = arrayToString(outputArray);
        }
        catch (Exception exception)
        {
            result = exception.getMessage();
        }

        if (errorHandler.containsErrors())
            return errorHandler.getPrimaryError();

        Text text = new Text(result);
        text.setFill(Color.WHITE);

        return new TextFlow(text);
    }

    private static String arrayToString(Object[] array)
    {
        StringBuilder builder = new StringBuilder();

        for (int i = 0; i < array.length; i++)
        {
            builder.append(array[i]);
            if (i != array.length - 1)
                builder.append(System.lineSeparator());
        }

        return builder.toString();
    }
}
