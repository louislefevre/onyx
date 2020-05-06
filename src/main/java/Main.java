import compilation.Compiler;
import ui.PrimaryInterface;

public class Main
{
    public static void main(String[] args)
    {
        Compiler compiler = new Compiler();
        compiler.run(false);
        PrimaryInterface primaryInterface = new PrimaryInterface();
        primaryInterface.launchInterface(args);
    }
}
