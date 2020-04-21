package source;

import lombok.Getter;

public class SourceLine
{
    @Getter
    private final int start;

    public SourceLine(int start)
    {
        this.start = start;
    }
}
