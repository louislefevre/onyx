package analysis.semantic;

public final class AnnotatedParseTree
{
    private final AnnotatedStatement statement;

    public AnnotatedParseTree(AnnotatedStatement statement)
    {
        this.statement = statement;
    }

    public AnnotatedStatement getStatement()
    {
        return this.statement;
    }
}
