package errors;

import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import source.SourceInput;

import java.util.LinkedList;
import java.util.Queue;

/**
 * The ErrorHandler class is used to store Error objects that have been generated during compilation.
 * <p>
 * As compilation proceeds errors may occur, which results in the creation of Error objects. These are passed to an
 * instance of ErrorHandler and stored within an internal LinkedList, from which the initial Error can be retrieved.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class ErrorHandler
{
    private final Queue<Error> errorsLog;
    private final SourceInput sourceInput;

    /**
     * Constructs an ErrorHandler object for storing Errors that occur during compilation.
     * <p>
     * Requires the initial compilation input so that errors can be located in the text.
     * <p>
     * A LinkedList is initialised on construction for storing the Error objects.
     *
     * @param sourceInput The source code input which is being compiled
     */
    public ErrorHandler(SourceInput sourceInput)
    {
        this.sourceInput = sourceInput;
        this.errorsLog = new LinkedList<>();
    }

    /**
     * Adds an Error object to be stored in the ErrorHandler.
     *
     * @param error The Error object to be stored
     */
    public void add(Error error)
    {
        errorsLog.add(error);
    }

    /**
     * Returns a boolean indicating whether the ErrorHandler contains any Error objects.
     * <p>
     * If true, the ErrorHandler contains at least one Error. If false, the ErrorHandler contains no errors.
     *
     * @return A boolean indicating if the ErrorHandler contain any errors
     */
    public boolean containsErrors()
    {
        return !errorsLog.isEmpty();
    }

    /**
     * Returns a TextFlow that contains various Text objects generated from portions of the ErrorMessage.
     * <p>
     * Only the first Error that the ErrorHandler received is returned. All other Errors are discarded, since they
     * are most likely false-positives that have only occurred as a result of the first Error being present.
     *
     * @return The TextFlow containing Text objects with information about the error
     */
    public TextFlow getPrimaryError()
    {
        Error error = errorsLog.element();
        Error.ErrorMessage errorMessage = error.getErrorMessage(sourceInput);

        Text infoText = new Text(errorMessage.getErrorInfo());
        Text prefixText = new Text(errorMessage.getPrefixSyntax());
        Text errorText = new Text(errorMessage.getErrorSyntax());
        Text suffixText = new Text(errorMessage.getSuffixSyntax());
        Text problemText = new Text(errorMessage.getProblem());
        Text solutionText = new Text(errorMessage.getSolution());

        infoText.setFill(Color.RED);
        prefixText.setFill(Color.GREY);
        errorText.setFill(Color.RED);
        suffixText.setFill(Color.GREY);
        problemText.setFill(Color.TOMATO);
        solutionText.setFill(Color.CHARTREUSE);

        return new TextFlow(infoText, prefixText, errorText, suffixText, problemText, solutionText);
    }
}
