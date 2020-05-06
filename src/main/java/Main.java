import compilation.Compiler;

public class Main
{
    public static void main(String[] args)
    {
        Compiler compiler = new Compiler();
        compiler.run(false, true);
    }
}
