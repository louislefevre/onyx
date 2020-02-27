import compilation.Compiler;

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
            Object output = compiler.compile(input);

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
