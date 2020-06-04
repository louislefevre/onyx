package source;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public final class SourceInput
{
    private final String sourceText;
    private final List<SourceLine> sourceLines;

    public SourceInput(String sourceText)
    {
        this.sourceText = sourceText;
        this.sourceLines = parseLines(sourceText);
    }

    public int length()
    {
        return sourceText.length();
    }

    public char charAt(int index)
    {
        return sourceText.charAt(index);
    }

    public String substring(int beginIndex)
    {
        return sourceText.substring(beginIndex);
    }

    public String substring(int beginIndex, int endIndex)
    {
        return sourceText.substring(beginIndex, endIndex);
    }

    public int getLineIndex(int position)
    {
        return searchLineList(sourceLines, 0, sourceLines.size() - 1, position);
    }

    private static List<SourceLine> parseLines(String text)
    {
        return splitLines(new ArrayList<>(), text, 0, 0);
    }

    private static int searchLineList(List<SourceLine> lineList, int lowerBound, int upperBound, int position)
    {
        if (lowerBound <= upperBound)
        {
            int index = lowerBound + (upperBound - lowerBound) / 2;
            int start = lineList.get(index).getStart();

            if (start == position)
                return index;
            if (start > position)
                return searchLineList(lineList, lowerBound, index - 1, position);
            else
                return searchLineList(lineList, index + 1, upperBound, position);
        }

        return lowerBound - 1;
    }

    private static List<SourceLine> splitLines(List<SourceLine> lineList, String text, int position, int lineStart)
    {
        if (position < text.length())
        {
            int lineBreakWidth = getLineBreakWidth(text, position);

            if (lineBreakWidth == 0)
                splitLines(lineList, text, ++position, lineStart);
            else
            {
                lineList.add(new SourceLine(lineStart, position - lineStart));
                position += lineBreakWidth;
                splitLines(lineList, text, position, position);
            }
        }
        else
        {
            if (position > lineStart)
                lineList.add(new SourceLine(lineStart, position - lineStart));
        }

        return lineList;
    }

    private static int getLineBreakWidth(String text, int position)
    {
        char escChar1 = text.charAt(position);
        char escChar2 = position + 1 >= text.length() ? '\0' : text.charAt(position + 1);

        if (escChar1 == '\r' && escChar2 == '\n') // Windows newline escape sequence
            return 2;
        if (escChar1 == '\r' || escChar1 == '\n') // Unix newline escape sequence
            return 1;

        return 0;
    }
}
