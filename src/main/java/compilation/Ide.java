package compilation;

import source.SourceOutput;

import java.util.Scanner;

public final class Ide
{
    public void run()
    {
        Pipeline pipeline = new Pipeline();
        StringBuilder builder = new StringBuilder();
        Scanner scanner = new Scanner(System.in);

        while(true)
        {
            if(builder.length() == 0)
                System.out.print("> ");
            else
                System.out.print("| ");

            String input = scanner.nextLine();

            if(builder.length() == 0 && input.isBlank())
                break;

            builder.append(input);
            String sourceText = builder.toString();

            SourceOutput sourceOutput = pipeline.compile(sourceText);

            if(!input.isBlank())
                continue;

            System.out.println(sourceOutput.getResult());
            builder = new StringBuilder();
        }
    }
}
