package analysis;

import analysis.lexical.SyntaxSpan;
import lombok.Getter;

@Getter
public class TextLine
{
    private final SourceText sourceText;
    private final int start;
    private final int length;
    private final int lengthIncludingLineBreak;
    private final SyntaxSpan syntaxSpan;
    private final SyntaxSpan spanIncludingLineBreak;

    public TextLine(SourceText sourceText, int start, int length, int lengthIncludingLineBreak)
    {
        this.sourceText = sourceText;
        this.start = start;
        this.length = length;
        this.lengthIncludingLineBreak = lengthIncludingLineBreak;
        this.syntaxSpan = new SyntaxSpan(start, length);
        this.spanIncludingLineBreak = new SyntaxSpan(start, lengthIncludingLineBreak);
    }
}
