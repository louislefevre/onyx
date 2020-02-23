package analysis.semantic;

public final class AnnotatedParseTree
{
    private final AnnotatedExpression expression;

    public AnnotatedParseTree(AnnotatedExpression expression)
    {
        this.expression = expression;
    }

    public AnnotatedExpression getExpression()
    {
        return this.expression;
    }
}
