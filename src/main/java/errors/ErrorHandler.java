package errors;

import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import lombok.Getter;
import source.SourceInput;
import source.SourceLine;
import util.ANSI;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public final class ErrorHandler
{
    private final List<Error> errorsLog;
    private SourceInput sourceInput;

    public ErrorHandler()
    {
        this.errorsLog = new ArrayList<>();
    }

    public void setSourceInput(SourceInput sourceInput)
    {
        errorsLog.clear();
        this.sourceInput = sourceInput;
    }

    public void addError(Error error)
    {
        errorsLog.add(error);
    }

    public boolean containsErrors()
    {
        return !errorsLog.isEmpty();
    }

    public String getErrors()
    {
        StringBuilder builder = new StringBuilder();

        for (ErrorMessage error : getErrorMessages())
        {
            String errorMessage = ANSI.RED + error.getErrorMessage();
            String prefixSyntax = ANSI.GREY + error.getPrefixSyntax();
            String errorSyntax = ANSI.RED + error.getErrorSyntax();
            String suffixSyntax = ANSI.GREY + error.getSuffixSyntax();
            String fullSyntax = errorMessage + prefixSyntax + errorSyntax + suffixSyntax + ANSI.RESET;

            builder.append(fullSyntax);
        }

        return builder.toString();
    }

    public TextFlow getPrimaryError()
    {
        ErrorMessage error = getErrorMessages().element();

        Text messageText = new Text(error.getErrorMessage());
        Text prefixText = new Text(error.getPrefixSyntax());
        Text errorText = new Text(error.getErrorSyntax());
        Text suffixText = new Text(error.getSuffixSyntax());

        messageText.setFill(Color.RED);
        prefixText.setFill(Color.GREY);
        errorText.setFill(Color.RED);
        suffixText.setFill(Color.GREY);

        return new TextFlow(messageText, prefixText, errorText, suffixText);
    }

    private Queue<ErrorMessage> getErrorMessages()
    {
        Queue<ErrorMessage> errorMessages = new LinkedList<>();

        for (Error error : errorsLog)
            errorMessages.add(new ErrorMessage(error, sourceInput));

        return errorMessages;
    }

    @Getter
    private static class ErrorMessage
    {
        private String errorMessage;
        private String prefixSyntax;
        private String errorSyntax;
        private String suffixSyntax;

        public ErrorMessage(Error error, SourceInput sourceInput)
        {
            initialiseErrorInfo(error, sourceInput);
        }

        private void initialiseErrorInfo(Error error, SourceInput sourceInput)
        {
            int errorStart = error.getSpan().getStart();
            int errorEnd = error.getSpan().getEnd();
            int lineIndex = sourceInput.getLineIndex(errorStart);

            SourceLine line = sourceInput.getSourceLines().get(lineIndex);
            int lineStart = line.getStart();
            int lineEnd = line.getEnd();
            int character = errorStart - lineStart + 1;

            String lineBreak = System.lineSeparator();
            String lineInfo = String.format(" (%1s,%2s): ", lineIndex + 1, character);
            String errorMessage = error.toString() + lineInfo + error.getErrorMessage();
            String prefixSyntax, errorSyntax, suffixSyntax;

            if (errorEnd > sourceInput.length() || lineEnd < errorEnd) // Handles unexpected EOF_TOKEN and LINE_BREAK_TOKEN errors
            {
                prefixSyntax = sourceInput.substring(lineStart, lineEnd);
                errorSyntax = "_";
                suffixSyntax = "";
            }
            else // Handles all other errors
            {
                prefixSyntax = sourceInput.substring(lineStart, errorStart);
                errorSyntax = sourceInput.substring(errorStart, errorEnd);
                suffixSyntax = sourceInput.substring(errorEnd, lineEnd);
            }

            prefixSyntax = prefixSyntax.replaceFirst("^\\s+", "");
            suffixSyntax = suffixSyntax.replaceFirst("\\s+$", "");

            errorMessage += lineBreak;
            suffixSyntax += lineBreak;

            this.errorMessage = errorMessage;
            this.prefixSyntax = "    " + prefixSyntax;
            this.errorSyntax = errorSyntax;
            this.suffixSyntax = suffixSyntax;
        }
    }
}
