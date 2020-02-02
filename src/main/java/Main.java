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
            String input = readInput();

            if(Utilities.isWhiteSpace(input) || Utilities.isNull(input))
                return;

            if(input.equals("#showTree"))
            {
                showTree = !showTree;
                System.out.println(showTree ? "Showing parse trees" : "Not showing parse trees");
                continue;
            }

            SyntaxTree syntaxTree = SyntaxTree.parse(input);

            if(showTree)
                prettyPrint(syntaxTree.getRoot(), "", true);

            if(!syntaxTree.getDiagnostics().isEmpty())
                showDiagnostics(syntaxTree);
            else
                evaluate(syntaxTree);
        }
    }

    private static String readInput()
    {
        System.out.print("> ");
        Scanner scanner = new Scanner(System.in);
        return scanner.nextLine();
    }

    private static void prettyPrint(SyntaxNode node, String indent, boolean isLast)
    {
        String marker = isLast ? "└──" : "├──";

        System.out.print(indent + marker + node.getKind());

        if(node instanceof SyntaxToken && ((SyntaxToken) node).getValue() != null)
            System.out.print(" " + ((SyntaxToken) node).getValue());

        System.out.println();
        indent += isLast ? "    " : "│   ";

        SyntaxNode lastChild = null;
        for(SyntaxNode child : node.getChildren())
            lastChild = child;

        for(SyntaxNode child : node.getChildren())
            prettyPrint(child, indent, child == lastChild);
    }

    private static void showDiagnostics(SyntaxTree syntaxTree)
    {
        for (String diagnostic : syntaxTree.getDiagnostics())
            System.out.println(diagnostic);
    }

    private static void evaluate(SyntaxTree syntaxTree)
    {
        Evaluator eval = new Evaluator(syntaxTree.getRoot());
        int result = eval.evaluate();
        System.out.println(result);
    }
}
