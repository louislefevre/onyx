package analysis.semantic;

public final class AnnotatedParseTree
{
    private final BoundExpression expression;

    public AnnotatedParseTree(BoundExpression expression)
    {
        this.expression = expression;
    }

    public BoundExpression getExpression()
    {
        return this.expression;
    }
}
