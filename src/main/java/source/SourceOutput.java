package source;

import compilation.generation.Evaluator;
import errors.ErrorHandler;
import exceptions.Exception;
import javafx.scene.Node;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;

/**
 * The SourceOutput class is used to store the output of the compilation process.
 * <p>
 * The output can be retrieved in two forms: its raw String form, or in the form of a TextFlow object. The former
 * is used for internal used (e.g. testing), whilst the latter is used for the GUI.
 * <p>
 * Note that depending on how the compiler was run and the input passed to it, no values may be returned by the
 * Evaluator and therefore by this class.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class SourceOutput
{
    private final TextFlow output;

    /**
     * Constructs a SourceOutput object initialised with the result calculated by the Evaluator.
     * <p>
     * On creation, the Evaluator is immediately called to evaluate and return the compilation result.
     *
     * @param evaluator The Evaluator used to generate the output
     * @param errorHandler The ErrorHandler used to report any errors that occurred during compilation
     */
    public SourceOutput(Evaluator evaluator, ErrorHandler errorHandler)
    {
        this.output = evaluateResult(evaluator, errorHandler);
    }

    /**
     * Return the compilation output as a TextFlow object.
     * <p>
     * If any errors occurred during compilation, they will be returned instead.
     * <p>
     * This method is intended to be used by the GUI.
     *
     * @return the output as a TextFlow
     */
    public TextFlow getOutput()
    {
        return output;
    }

    /**
     * Return the compilation output as a String.
     * <p>
     * If any errors occurred during compilation, they will be returned instead.
     * <p>
     * This method is intended for internal use.
     *
     * @return the output as a String
     */
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
