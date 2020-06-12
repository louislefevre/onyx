package source;

import lombok.Getter;

/**
 * The SourceSpan class is used to store information about the location of Expression and Statement objects relative
 * to the source code.
 * <p>
 * It holds information about the start index, end index, and total length of each line.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class SourceSpan
{
    private final int start;
    private final int length;
    private final int end;

    /**
     * Constructs a SourceSpan object for storing information about each Expression or Statements location.
     * <p>
     * The end index is automatically calculated by adding the start and length together.
     *
     * @param start The start index of the span, relative to the source code
     * @param length The length of the span
     */
    public SourceSpan(int start, int length)
    {
        this.start = start;
        this.length = length;
        this.end = start + length;
    }

    /**
     * Creates and returns a SourceSpan object for storing information about each Expression or Statements location.
     * <p>
     * This method differs from the SourceSpan constructor as it allows for the creation of an instance where the end
     * index can be manually defined.
     *
     * @param start The start index of the span, relative to the source code
     * @param end The end index of the span, relative to the source code
     * @return A SourceSpan object
     */
    public static SourceSpan inRange(int start, int end)
    {
        return new SourceSpan(start, end-start);
    }
}
