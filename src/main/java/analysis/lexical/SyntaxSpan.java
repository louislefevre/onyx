package analysis.lexical;

import lombok.Getter;

public class SyntaxSpan
{
    @Getter private final int start;
    @Getter private final int length;
    @Getter private final int end;

    public SyntaxSpan(int start, int length)
    {
        this.start = start;
        this.length = length;
        this.end = start + length;
    }
}
