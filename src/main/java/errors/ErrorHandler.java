package errors;

import lombok.Getter;
import org.jetbrains.annotations.TestOnly;
import source.SourceDisplay;
import source.SourceInput;

import java.util.ArrayList;
import java.util.List;

@Getter
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

    @TestOnly
    public String getErrors()
    {
        return new SourceDisplay(null, sourceInput, this).getDecoratedErrors();
    }
}
