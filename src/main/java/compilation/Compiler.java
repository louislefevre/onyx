package compilation;

import source.SourceOutput;
import ui.SceneManager;

import java.util.Scanner;

public class Compiler
{
    public void run(boolean replMode, boolean guiMode)
    {
        if (guiMode)
            guiMode();
        else
            consoleMode(replMode);
    }

    private void guiMode()
    {
        SceneManager.launchInterface();
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
            output = sourceOutput.getDecoratedOutput();
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
            Object output = sourceOutput.getDecoratedOutput();
            System.out.println(output);
        }
    }
}
