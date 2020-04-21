package compilation;

import source.SourceOutput;

import java.util.Scanner;

public final class Repl
{
    public Repl()
    {
    }

    public void run()
    {
        Compiler compiler = new Compiler();

        String input = readInput();

        SourceOutput output = compiler.compile(input);

        System.out.println(output.getResult());

    }

    private static String readInput()
    {
        StringBuilder builder = new StringBuilder();
        Scanner scanner = new Scanner(System.in);

        System.out.print("> ");
        String line;
        while(true)
        {
            line = scanner.nextLine();

            if(line.isBlank())
                break;

            builder.append(line);
            builder.append(System.getProperty("line.separator"));
            System.out.print("| ");
        }

        return builder.toString();
    }
}











