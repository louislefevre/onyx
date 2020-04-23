package errors;

import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import source.SourceInput;
import source.SourceLine;

import java.util.ArrayList;
import java.util.List;

public final class ErrorHandler
{
    private final List<Error> errorsLog;
    private final SourceInput sourceInput;

    public ErrorHandler(SourceInput sourceInput)
    {
        this.errorsLog = new ArrayList<>();
        this.sourceInput = sourceInput;
    }

    public void addError(Error error)
    {
        this.errorsLog.add(error);
    }

    public boolean errorsPresent()
    {
        return !this.errorsLog.isEmpty();
    }

    public List<Text> outputErrors()
    {
        List<Text> lines = new ArrayList<>();
        for (Error error : this.errorsLog)
        {
            int errorStart = error.getSpan().getStart();
            int errorEnd = error.getSpan().getEnd();
            int lineIndex = this.sourceInput.getLineIndex(errorStart);

            SourceLine line = this.sourceInput.getSourceLines().get(lineIndex);
            int lineStart = line.getStart();
            int lineEnd = line.getEnd();
            int character = errorStart - lineStart + 1;

            String lineInfo = String.format(" (%1s,%2s): ", lineIndex + 1, character);
            String errorMessage = error.toString() + lineInfo + error.getErrorMessage();

            String prefixSyntax, errorSyntax, suffixSyntax;
            if (errorEnd > this.sourceInput.length()) // Handles unexpected EOF_TOKEN errors
            {
                prefixSyntax = this.sourceInput.substring(lineStart, lineEnd);
                errorSyntax = "_";
                suffixSyntax = "";
            }
            else // Handles all other errors
            {
                prefixSyntax = this.sourceInput.substring(lineStart, errorStart);
                errorSyntax = this.sourceInput.substring(errorStart, errorEnd);
                suffixSyntax = this.sourceInput.substring(errorEnd, lineEnd);
            }

            lines.add(createText(errorMessage + System.getProperty("line.separator") + "    ", Color.RED));

            if (!prefixSyntax.isBlank())
                lines.add(createText(prefixSyntax, Color.GREY));

            if (!errorSyntax.isBlank())
                lines.add(createText(errorSyntax, Color.RED));

            if (!suffixSyntax.isBlank())
                lines.add(createText(suffixSyntax, Color.GREY));

            lines.add(new Text(System.getProperty("line.separator")));
        }
        return lines;
    }

    private static Text createText(String str, Color color)
    {
        Text text = new Text(str);
        text.setFill(color);
        return text;
    }
}
