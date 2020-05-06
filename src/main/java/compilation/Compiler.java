package compilation;

import source.SourceOutput;
import ui.PrimaryInterface;

import java.util.Scanner;

public class Compiler
{
    public void run(boolean replMode, boolean guiMode)
    {
        if (guiMode)
            guiMode(replMode);
        else
            consoleMode(replMode);
    }

    private void guiMode(boolean replMode)
    {
        PrimaryInterface primaryInterface = new PrimaryInterface();
        primaryInterface.launchInterface();
    }

    private void consoleMode(boolean replMode)
    {
        Scanner scanner = new Scanner(System.in);
        Pipeline pipeline = new Pipeline();

        if (replMode)
            runConsoleREPL(pipeline, scanner);
        else
            runConsoleIDE(pipeline, scanner);

        pipeline.printParseTree();
        pipeline.printSymbolTable();

        scanner.close();
    }

    private void runConsoleIDE(Pipeline pipeline, Scanner scanner)
    {
        StringBuilder builder = new StringBuilder();
        Object output = new Object();

        while (true)
        {
            System.out.print("| ");
            String input = scanner.nextLine();

            if (input.equals("END"))
                break;

            builder.append(input);
            builder.append(System.getProperty("line.separator"));
            String sourceText = builder.toString();

            SourceOutput sourceOutput = pipeline.compile(sourceText);
            output = sourceOutput.getResult();
        }

        System.out.println(output);
    }

    private void runConsoleREPL(Pipeline pipeline, Scanner scanner)
    {
        pipeline.enableReplMode();

        while (true)
        {
            System.out.print("> ");
            String input = scanner.nextLine();

            if (input.isBlank())
                break;

            SourceOutput sourceOutput = pipeline.compile(input);
            Object output = sourceOutput.getResult();
            System.out.println(output);
        }
    }
}
