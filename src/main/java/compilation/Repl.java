package compilation;

import source.SourceInput;
import source.SourceOutput;

import java.util.Scanner;

public final class Repl
{
    public void run()
    {
        Compiler compiler = new Compiler();

        while (true)
        {
            SourceInput input = readInput();

            if (input.isBlank())
                break;

            SourceOutput output = compiler.compile(input);

            System.out.println(output.getResult());
        }
    }

    private static SourceInput readInput()
    {
        StringBuilder builder = new StringBuilder();
        Scanner scanner = new Scanner(System.in);

        System.out.print("> ");
        String line;
        while (true)
        {
            line = scanner.nextLine();

            if (line.isBlank())
                break;

            builder.append(line);
            builder.append(System.getProperty("line.separator"));
            System.out.print("| ");
        }

        return new SourceInput(builder.toString());
    }
}











