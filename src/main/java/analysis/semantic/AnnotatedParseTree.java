package analysis.semantic;

import lombok.Getter;

@Getter
public final class AnnotatedParseTree
{
    private final AnnotatedStatement statement;

    public AnnotatedParseTree(AnnotatedStatement statement)
    {
        this.statement = statement;
    }
}
