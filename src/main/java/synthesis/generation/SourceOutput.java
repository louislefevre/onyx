package synthesis.generation;

public final class SourceOutput
{
    private final Object output;

    public SourceOutput(Evaluator evaluator)
    {
        this.output = evaluator.evaluate();
    }

    public Object getOutput()
    {
        return output;
    }
}
