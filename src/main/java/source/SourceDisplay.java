package source;

import errors.Error;
import errors.ErrorHandler;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import lombok.Getter;
import util.ANSI;

import java.util.ArrayList;
import java.util.List;

public final class SourceDisplay
{
    private final SourceInput sourceInput;
    private final Object result;
    private final ErrorHandler errorHandler;

    public SourceDisplay(Object result, SourceInput sourceInput, ErrorHandler errorHandler)
    {
        this.result = result;
        this.sourceInput = sourceInput;
        this.errorHandler = errorHandler;
    }

    public String getErrors()
    {
        StringBuilder builder = new StringBuilder();

        for(ErrorDisplay error : processErrors())
        {
            String errorMessage = error.getErrorMessage();
            String prefixSyntax = error.getPrefixSyntax();
            String errorSyntax  = error.getErrorSyntax();
            String suffixSyntax = error.getSuffixSyntax();
            String fullSyntax   = errorMessage + "    " + prefixSyntax + errorSyntax + suffixSyntax;

            builder.append(fullSyntax);
        }

        return builder.toString();
    }

    public String getDecoratedErrors()
    {
        StringBuilder builder = new StringBuilder();

        for(ErrorDisplay error : processErrors())
        {
            String errorMessage = ANSI.RED  + error.getErrorMessage();
            String prefixSyntax = ANSI.GREY + error.getPrefixSyntax();
            String errorSyntax  = ANSI.RED  + error.getErrorSyntax();
            String suffixSyntax = ANSI.GREY + error.getSuffixSyntax();
            String fullSyntax   = errorMessage + "    " + prefixSyntax + errorSyntax + suffixSyntax + ANSI.RESET;

            builder.append(fullSyntax);
        }

        return builder.toString();
    }

    public List<Text> getTextErrors()
    {
        List<Text> lines = new ArrayList<>();

        for(ErrorDisplay error : processErrors())
        {
            String errorMessage = error.getErrorMessage();
            String prefixSyntax = "    " + error.getPrefixSyntax();
            String errorSyntax  = error.getErrorSyntax();
            String suffixSyntax = error.getSuffixSyntax();

            Text messageText = new Text(errorMessage);
            Text prefixText = new Text(prefixSyntax);
            Text errorText = new Text(errorSyntax);
            Text suffixText = new Text(suffixSyntax);

            messageText.setFill(Color.RED);
            prefixText.setFill(Color.GREY);
            errorText.setFill(Color.RED);
            suffixText.setFill(Color.GREY);

            lines.add(messageText);
            lines.add(prefixText);
            lines.add(errorText);
            lines.add(suffixText);
        }

        return lines;
    }

    public List<Text> getTextResult()
    {
        if(result == null)
            return null;

        Text text = new Text(result.toString());
        text.setFill(Color.WHITE);
        List<Text> list = new ArrayList<>();
        list.add(text);

        return list;
    }

    private List<ErrorDisplay> processErrors()
    {
        List<ErrorDisplay> errors = new ArrayList<>();

        for(Error error : errorHandler.getErrorsLog())
            errors.add(getErrorInfo(error));

        return errors;
    }

    private ErrorDisplay getErrorInfo(Error error)
    {
        int errorStart = error.getSpan().getStart();
        int errorEnd = error.getSpan().getEnd();
        int lineIndex = sourceInput.getLineIndex(errorStart);

        SourceLine line = sourceInput.getSourceLines().get(lineIndex);
        int lineStart = line.getStart();
        int lineEnd = line.getEnd();
        int character = errorStart - lineStart + 1;

        String lineBreak = System.getProperty("line.separator");
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

        return new ErrorDisplay(errorMessage, prefixSyntax, errorSyntax, suffixSyntax);
    }

    @Getter
    private class ErrorDisplay
    {
        private final String errorMessage;
        private final String prefixSyntax;
        private final String errorSyntax;
        private final String suffixSyntax;

        public ErrorDisplay(String errorMessage, String prefixSyntax, String errorSyntax, String suffixSyntax)
        {
            this.errorMessage = errorMessage;
            this.prefixSyntax = prefixSyntax;
            this.errorSyntax = errorSyntax;
            this.suffixSyntax = suffixSyntax;
        }
    }
}
