package source;

import errors.ErrorHandler;
import lombok.Getter;
import symbols.SymbolTable;

import java.util.ArrayList;
import java.util.List;

@Getter
public final class SourceInput
{
    private final String sourceText;
    private final SymbolTable symbolTable;
    private final ErrorHandler errorHandler;
    private final boolean replMode;
    private final List<SourceLine> sourceLines;

    public SourceInput(String sourceText, SymbolTable symbolTable, ErrorHandler errorHandler, boolean replMode)
    {
        this.sourceText = sourceText;
        this.symbolTable = symbolTable;
        this.errorHandler = errorHandler;
        this.replMode = replMode;
        this.sourceLines = parseLines(sourceText);
        errorHandler.setSourceInput(this);
    }

    public int length()
    {
        return sourceText.length();
    }

    public String substring(int beginIndex, int endIndex)
    {
        return sourceText.substring(beginIndex, endIndex);
    }

    public int getLineIndex(int position)
    {
        return searchLineList(sourceLines, 0, sourceLines.size() - 1, position);
    }

    private List<SourceLine> parseLines(String text)
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
