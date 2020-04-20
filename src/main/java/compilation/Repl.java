package compilation;

import synthesis.generation.SourceOutput;

import java.util.Scanner;

public final class Repl
{
    public Repl()
    {
    }

    public void run()
    {
        Compiler compiler = new Compiler();

        while (true)
        {
            String input = readInput();
            if (input.isBlank())
                return;

            SourceOutput output = compiler.compile(input);

            System.out.println(output.getResult());
        }
    }

    private static String readInput()
    {
        System.out.print("> ");
        Scanner scanner = new Scanner(System.in);
        return scanner.nextLine();
    }
}
