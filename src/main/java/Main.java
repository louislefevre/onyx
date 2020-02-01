package main.java;

import main.java.codeanalysis.*;
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

            Parser parser = new Parser(input);
            ExpressionSyntax expression = parser.parse();
            prettyPrint(expression, "");
        }
    }

    private static void prettyPrint(SyntaxNode node, String indent)
    {
        //String marker = isLast ? "└──" : "├──";

        System.out.print(indent);
        //System.out.print(marker);
        System.out.print(node.getKind());

        if(node instanceof SyntaxToken && ((SyntaxToken) node).getValue() != null)
        {
            System.out.print(" ");
            System.out.print(((SyntaxToken) node).getValue());
        }

        System.out.println(" ");
        indent += "    ";

        //indent += isLast ? "    " : "│   ";

        //SyntaxNode lastChild = node.getChildren().;

        for(SyntaxNode child : node.getChildren())
        {
            prettyPrint(child, indent);
        }
    }
}
