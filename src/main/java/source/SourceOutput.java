package source;

import errors.ErrorHandler;
import generation.Evaluator;
import javafx.scene.Node;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;

public final class SourceOutput
{
    private final Object result;
    private final ErrorHandler errorHandler;

    public SourceOutput(Evaluator evaluator, ErrorHandler errorHandler)
    {
        this.result = evaluator.getEvaluation();
        this.errorHandler = errorHandler;
    }

    public TextFlow getOutput()
    {
        if (errorHandler.containsErrors())
            return errorHandler.getPrimaryError();

        Text text = new Text(result.toString());
        text.setFill(Color.WHITE);

        return new TextFlow(text);
    }

    public String getRawOutput()
    {
        TextFlow textFlow = getOutput();
        StringBuilder builder = new StringBuilder();

        for (Node child : textFlow.getChildren())
            builder.append(((Text) child).getText());

        return builder.toString();
    }
}
