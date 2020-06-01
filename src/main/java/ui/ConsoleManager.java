package ui;

import compilation.Pipeline;
import source.SourceOutput;

import java.util.Scanner;

public final class ConsoleManager
{
    public static void startConsole()
    {
        System.out.print("COMMANDS: ");
        Scanner scanner = new Scanner(System.in);
        String input = scanner.nextLine().toLowerCase();

        Pipeline pipeline = new Pipeline();
        if (input.contains("repl") || input.contains("r"))
            runREPL(pipeline, scanner);
        else
            runCompiler(pipeline, scanner);

        if (input.contains("parsetree") || input.contains("pt"))
            pipeline.printParseTree();
        if (input.contains("symboltable") || input.contains("st"))
            pipeline.printSymbolTable();

        scanner.close();
    }

    private static void runCompiler(Pipeline pipeline, Scanner scanner)
    {
        StringBuilder builder = new StringBuilder();
        Object output = "";

        while (true)
        {
            System.out.print("| ");
            String input = scanner.nextLine();

            if (input.isBlank())
                break;

            builder.append(input);
            builder.append(System.getProperty("line.separator"));
            String sourceText = builder.toString();

            SourceOutput sourceOutput = pipeline.compile(sourceText);
            output = sourceOutput.getOutput();
        }

        System.out.println(output);
    }

    private static void runREPL(Pipeline pipeline, Scanner scanner)
    {
        pipeline.enableReplMode();

        while (true)
        {
            System.out.print("> ");
            String input = scanner.nextLine();

            if (input.isBlank())
                break;

            SourceOutput sourceOutput = pipeline.compile(input);
            Object output = sourceOutput.getOutput();
            System.out.println(output);
        }
    }
}
