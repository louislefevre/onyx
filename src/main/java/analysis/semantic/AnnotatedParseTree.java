package analysis.semantic;

public final class AnnotatedParseTree
{
    private final AnnotatedStatement annotatedStatement;

    public AnnotatedParseTree(AnnotatedStatement annotatedStatement)
    {
        this.annotatedStatement = annotatedStatement;
    }

    public AnnotatedStatement getAnnotatedStatement()
    {
        return this.annotatedStatement;
    }
}
