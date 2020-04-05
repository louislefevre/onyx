package analysis.lexical;

import lombok.Getter;

@Getter
public final class SyntaxSpan
{
    private final int start;
    private final int length;
    private final int end;

    public SyntaxSpan(int start, int length)
    {
        this.start = start;
        this.length = length;
        this.end = start + length;
    }
}
