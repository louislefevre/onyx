import compilation.Compilation;

public class Main
{
    public static void main(String[] args)
    {
        boolean devMode = args.length > 0 && args[0].equals("dev");

        Compilation compilation = new Compilation();
        compilation.run(devMode);
    }
}
