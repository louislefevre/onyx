package compilation;

import source.SourceOutput;

import java.util.Scanner;

public final class Repl
{
    public void run()
    {
        Pipeline pipeline = new Pipeline();

        while (true)
        {
            String input = readInput();

            if (input.isBlank())
                break;

            SourceOutput output = pipeline.compile(input);

            System.out.println(output.getResult());
        }
    }

    private static String readInput()
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

        return builder.toString();
    }
}
