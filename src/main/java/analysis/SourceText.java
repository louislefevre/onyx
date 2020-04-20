package analysis;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

public final class SourceText
{
    @Getter
    private final List<TextLine> lines;
    private final String text;

    public SourceText(String text)
    {
        this.text = text;
        this.lines = parseLines(this, text);
    }

    public String substring(int beginIndex, int endIndex)
    {
        return this.text.substring(beginIndex, endIndex);
    }

    public String substring(int beginIndex)
    {
        return this.text.substring(beginIndex);
    }

    public char charAt(int index)
    {
        return this.text.charAt(index);
    }

    public int length()
    {
        return this.text.length();
    }

    public int getLineIndex(int position)
    {
        int lower = 0;
        int upper = this.lines.size() - 1;

        while (lower <= upper)
        {
            int index = lower + (upper - lower) / 2;
            int start = this.lines.get(index).getStart();

            if (position == start)
                return index;

            if (start > position)
                upper = index - 1;
            else
                lower = index + 1;
        }

        return lower - 1;
    }

    private static List<TextLine> parseLines(SourceText sourceText, String text)
    {
        List<TextLine> result = new ArrayList<>();
        int position = 0;
        int lineStart = 0;

        while (position < text.length())
        {
            int lineBreakWidth = getLineBreakWidth(text, position);

            if (lineBreakWidth == 0)
            {
                position++;
            }
            else
            {
                addLine(result, sourceText, position, lineStart, lineBreakWidth);
                position += lineBreakWidth;
                lineStart = position;
            }
        }

        if (position > lineStart)
        {
            addLine(result, sourceText, position, lineStart, 0);
        }

        return result;
    }

    private static void addLine(List<TextLine> result, SourceText sourceText, int position, int lineStart,
                                int lineBreakWidth)
    {
        int lineLength = position - lineStart;
        int lineLengthIncludingLineBreak = lineLength + lineBreakWidth;
        TextLine line = new TextLine(sourceText, lineStart, lineLength, lineLengthIncludingLineBreak);
        result.add(line);
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
