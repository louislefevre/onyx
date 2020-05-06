package source;

import lombok.Getter;

@Getter
public final class SourceSpan
{
    private final int start;
    private final int length;
    private final int end;

    public SourceSpan(int start, int length)
    {
        this.start = start;
        this.length = length;
        this.end = start + length;
    }

    public static SourceSpan inRange(int start, int end)
    {
        return new SourceSpan(start, end-start);
    }
}
