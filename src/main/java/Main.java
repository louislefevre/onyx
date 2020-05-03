import compilation.Ide;
import compilation.Repl;

public class Main
{
    public static void main(String[] args)
    {
        boolean replMode = true;

        if (replMode)
        {
            Repl repl = new Repl();
            repl.run();
        }
        else
        {
            Ide ide = new Ide();
            ide.run();
        }
    }
}
