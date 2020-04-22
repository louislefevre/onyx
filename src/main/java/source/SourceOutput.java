package source;

import errors.ErrorHandler;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import synthesis.generation.Evaluator;

import java.util.ArrayList;
import java.util.List;

public final class SourceOutput
{
    private final Object result;
    private final ErrorHandler errorHandler;

    public SourceOutput(Evaluator evaluator, ErrorHandler errorHandler)
    {
        this.result = evaluator.getEvaluation();
        this.errorHandler = errorHandler;
    }

    public List<Text> getResult()
    {
        if (this.errorHandler.errorsPresent())
            return this.errorHandler.outputErrors();

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
