package source;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

/**
 * The SourceInput class is used to store the source code to be used in compilation.
 * <p>
 * Various methods are provided to perform operations on the source code during compilation, many of which are just
 * extensions of String methods. Though other functions are provided which allow the text to be searched, and the
 * line index of a particular substring can be retrieved.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class SourceInput
{
    private final String sourceText;
    private final List<SourceLine> sourceLines;

    /**
     * Constructs a SourceInput object for storing the source code before compilation.
     * <p>
     * The source text is initially cleaned, which (if necessary) adds a line break to the end of the text in order
     * to avoid issues with compilation. The text is then parsed into SourceLine objects, which are used to identify
     * the range and order of each individual line
     *
     * @param sourceText The source code input
     */
    public SourceInput(String sourceText)
    {
        this.sourceText = cleanText(sourceText);
        this.sourceLines = parseLines(sourceText);
    }

    /**
     * Returns the length of the source text
     *
     * @return The source text length
     */
    public int length()
    {
        return sourceText.length();
    }

    /**
     * Returns a char found at the specified index.
     *
     * @param index The index of the character
     * @return The char at the indexes position
     */
    public char charAt(int index)
    {
        return sourceText.charAt(index);
    }

    /**
     * Return a String from a specified index to the end of the text.
     *
     * @param beginIndex The beginning of the substring
     * @return A substring from beginIndex to the end of the text
     */
    public String substring(int beginIndex)
    {
        return sourceText.substring(beginIndex);
    }

    /**
     * Return a String from a beginning index to an end index.
     *
     * @param beginIndex The beginning of the substring
     * @param endIndex The end of the substring
     * @return A substring from beginIndex to endIndex
     */
    public String substring(int beginIndex, int endIndex)
    {
        return sourceText.substring(beginIndex, endIndex);
    }

    /**
     * Returns the line that a character is on by using its position.
     * <p>
     * By using the position of the character, and line index can be retrieved.
     *
     * @param position The position of the character in the text
     * @return An int representing the line index
     */
    public int getLineIndex(int position)
    {
        return searchLineList(sourceLines, 0, sourceLines.size() - 1, position);
    }

    private static String cleanText(String text)
    {
        if (text.isBlank())
            return text;

        String lastChar = text.substring(text.length() - 1);
        if (!(lastChar.equals(System.lineSeparator()) || lastChar.equals("\n")))
            text += System.lineSeparator();

        return text;
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
