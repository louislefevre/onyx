package errors;

import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import source.SourceInput;

import java.util.LinkedList;
import java.util.Queue;

public final class ErrorHandler
{
    private final Queue<Error> errorsLog;
    private final SourceInput sourceInput;

    public ErrorHandler(SourceInput sourceInput)
    {
        this.sourceInput = sourceInput;
        this.errorsLog = new LinkedList<>();
    }

    public void add(Error error)
    {
        errorsLog.add(error);
    }

    public boolean containsErrors()
    {
        return !errorsLog.isEmpty();
    }

    public TextFlow getPrimaryError()
    {
        Error error = errorsLog.element();
        Error.ErrorMessage errorMessage = error.getErrorMessage(sourceInput);

        Text messageText = new Text(errorMessage.getErrorInfo());
        Text prefixText = new Text(errorMessage.getPrefixSyntax());
        Text errorText = new Text(errorMessage.getErrorSyntax());
        Text suffixText = new Text(errorMessage.getSuffixSyntax());

        messageText.setFill(Color.RED);
        prefixText.setFill(Color.GREY);
        errorText.setFill(Color.RED);
        suffixText.setFill(Color.GREY);

        return new TextFlow(messageText, prefixText, errorText, suffixText);
    }
}
