package source;

import lombok.Getter;

/**
 * The SourceLine class is used to store information about each individual line in the source code.
 * <p>
 * It holds information about the start index, end index, and total length of each line.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class SourceLine
{
    private final int start;
    private final int length;
    private final int end;

    /**
     * Constructs a SourceLine object for storing information about each line of source code.
     * <p>
     * The end index is automatically calculated by adding the start and length together.
     *
     * @param start The start index of the line, relative to the source code
     * @param length The length of the line
     */
    public SourceLine(int start, int length)
    {
        this.start = start;
        this.length = length;
        this.end = start + length;
    }
}
