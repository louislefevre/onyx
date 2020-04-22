import compilation.Repl;
import ui.PrimaryInterface;

public class Main
{
    public static void main(String[] args)
    {
        PrimaryInterface primaryInterface = new PrimaryInterface();
        primaryInterface.run(args);

        //Repl repl = new Repl();
        //repl.run();
    }
}
