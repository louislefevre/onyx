import analysis.Compiler;

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

            Compiler compiler = new Compiler(input);
            int output = compiler.compile();
            System.out.println(output);
        }
    }

    private static String readInput()
    {
        System.out.print("> ");
        Scanner scanner = new Scanner(System.in);
        return scanner.nextLine();
    }
}
