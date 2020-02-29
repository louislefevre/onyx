package errors;

import lombok.Getter;

public class TextSpan
{
    @Getter private final int start;
    @Getter private final int length;
    @Getter private final int end;

    public TextSpan(int start, int length)
    {
        this.start = start;
        this.length = length;
        this.end = start + length;
    }
}
