package ui;

import compilation.Compiler;
import source.SourceOutput;

import java.util.Scanner;

public final class ConsoleManager
{
    public static void startConsole()
    {
        System.out.print("COMMANDS: ");
        Scanner scanner = new Scanner(System.in);
        String input = scanner.nextLine().toLowerCase();

        Compiler compiler = new Compiler();

        if (input.contains("repl") || input.contains("r"))
            runREPL(compiler, scanner);
        else
            runCompiler(compiler, scanner);

        if (input.contains("parsetree") || input.contains("pt"))
            compiler.printParseTree();
        if (input.contains("symboltable") || input.contains("st"))
            compiler.printSymbolTable();

        scanner.close();
    }

    private static void runCompiler(Compiler compiler, Scanner scanner)
    {
        StringBuilder builder = new StringBuilder();

        while (true)
        {
            System.out.print("| ");
            String input = scanner.nextLine();

            if (input.isBlank())
                break;

            builder.append(input);
            builder.append(System.lineSeparator());
        }

        String input = builder.toString();
        SourceOutput output = compiler.compileInput(input);

        System.out.println(output.getOutput());
    }

    private static void runREPL(Compiler compiler, Scanner scanner)
    {
        compiler.toggleReplMode();

        while (true)
        {
            System.out.print("> ");
            String input = scanner.nextLine();

            if (input.isBlank())
                break;

            SourceOutput output = compiler.compileInput(input);
            System.out.println(output.getOutput());
        }
    }
}
