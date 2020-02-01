package main.java;

import main.java.codeanalysis.Lexer;
import main.java.codeanalysis.SyntaxKind;
import main.java.codeanalysis.SyntaxToken;
import main.java.codeanalysis.Utilities;

import java.util.Scanner;

public class Main
{
    public static void main(String[] args)
    {
        boolean showTree = false;

        while(true)
        {
            System.out.print("> ");
            Scanner scanner = new Scanner(System.in);
            String input = scanner.nextLine();

            if(Utilities.isWhiteSpace(input) || Utilities.isNull(input))
                return;

            if(input.equals("#showTree"))
            {
                showTree = !showTree;
                System.out.println(showTree ? "Showing parse trees" : "Not showing parse trees");
                continue;
            }
            else if(input.equals("#cls"))
            {
                // Clear console
                continue;
            }

            Lexer lexer = new Lexer(input);
            while(true)
            {
                SyntaxToken token = lexer.nextToken();
                if(token.getKind() == SyntaxKind.EndOfFileToken)
                    break;

                System.out.print(String.format("%1$s: '%1$s'", token.getKind(), token.getText()));
                if(token.getValue() != null)
                    System.out.print(String.format("%s", token.getValue()));

                System.out.println();
            }
        }
    }
}
