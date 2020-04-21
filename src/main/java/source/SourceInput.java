package source;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

public final class SourceInput
{
    @Getter
    private final List<SourceLine> sourceLines;
    private final String sourceText;

    public SourceInput(String sourceText)
    {
        this.sourceText = sourceText;
        this.sourceLines = parseLines(sourceText);
    }

    public char charAt(int index)
    {
        return this.sourceText.charAt(index);
    }

    public int length()
    {
        return this.sourceText.length();
    }

    public String substring(int beginIndex, int endIndex)
    {
        return this.sourceText.substring(beginIndex, endIndex);
    }

    public int getLineIndex(int position)
    {
        int lower = 0;
        int upper = this.sourceLines.size() - 1;

        while (lower <= upper)
        {
            int index = lower + (upper - lower) / 2;
            int start = this.sourceLines.get(index).getStart();

            if (start == position)
                return index;

            if (start > position)
                upper = index - 1;
            else
                lower = index + 1;
        }

        return lower - 1;
    }

    private static List<SourceLine> parseLines(String text)
    {
        List<SourceLine> result = new ArrayList<>();
        int position = 0, lineStart = 0;

        while (position < text.length())
        {
            int lineBreakWidth = getLineBreakWidth(text, position);

            if (lineBreakWidth == 0)
            {
                position++;
                continue;
            }

            result.add(new SourceLine(lineStart, position - lineStart));
            position += lineBreakWidth;
            lineStart = position;
        }

        if (position > lineStart)
            result.add(new SourceLine(lineStart, position - lineStart));

        return result;
    }

    private static int getLineBreakWidth(String text, int i)
    {
        char c = text.charAt(i);
        char l = i + 1 >= text.length() ? '\0' : text.charAt(i + 1);

        if (c == '\r' && l == '\n')
            return 2;
        if (c == '\r' || c == '\n')
            return 1;

        return 0;
    }
}
