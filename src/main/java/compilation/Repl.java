package compilation;

import symbols.SymbolTable;
import synthesis.generation.SourceOutput;

import java.util.Scanner;

public final class Repl
{
    public Repl() { }

    public void run()
    {
        SymbolTable symbolTable = new SymbolTable();
        Compiler compiler = new Compiler(symbolTable);

        while(true)
        {
            String input = readInput();
            if(input.isBlank())
                return;

            SourceOutput output = compiler.compile(input);

            if(!output.compilationFailed())
                System.out.println(output.getResult());
            System.out.println();
        }
    }

    private static String readInput()
    {
        System.out.print("> ");
        Scanner scanner = new Scanner(System.in);
        return scanner.nextLine();
    }
}
