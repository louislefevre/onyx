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
            String sourceText = readInput();

            if (sourceText.isBlank())
                break;

            SourceOutput output = pipeline.compile(sourceText);

            //pipeline.printParseTree();
            //pipeline.printSymbolTable();

            System.out.println(output.getResult());
            System.out.println();
        }
    }

    private static String readInput()
    {
        StringBuilder builder = new StringBuilder();
        Scanner scanner = new Scanner(System.in);
        System.out.print("> ");

        while (true)
        {
            String line = scanner.nextLine();

            if (line.isBlank())
                break;

            builder.append(line);
            builder.append(System.getProperty("line.separator"));
            System.out.print("| ");
        }

        return builder.toString();
    }
}
