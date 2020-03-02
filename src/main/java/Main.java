import compilation.Compiler;
import synthesis.generation.SourceOutput;

import java.util.Scanner;

public class Main
{
    public static void main(String[] args)
    {
        while(true)
        {
            String input = readInput();
            if(input.isBlank())
                return;

            Compiler compiler = new Compiler();
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
