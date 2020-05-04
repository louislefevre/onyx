package analysis.syntax;

import analysis.lexical.Token;
import org.jetbrains.annotations.TestOnly;
import util.ANSI;

import java.util.List;

public final class ParseTree
{
    private final Statement statement;
    private static Statement staticStatement;

    public ParseTree(Statement statement)
    {
        this.statement = statement;
        staticStatement = statement;
    }

    public Statement getStatement()
    {
        return statement;
    }

    @TestOnly
    public static void printParseTree()
    {
        printTree(staticStatement, "", true);
    }

    private static void printTree(Object node, String indent, boolean isLast)
    {
        String marker = ANSI.GREY;
        marker += isLast ? "└──" : "├──";

        if (node instanceof Statement)
            System.out.print(indent + marker + ANSI.CYAN + ((Statement) node).getStatementType());
        else if (node instanceof Expression)
            System.out.print(indent + marker + ANSI.CYAN + ((Expression) node).getExpressionType());
        else if (node instanceof Token)
            System.out.print(indent + marker + ANSI.BRIGHT_RED + ((Token) node).getType());

        if (node instanceof Token && ((Token) node).getValue() != null)
            System.out.print(ANSI.RED + " (" + ((Token) node).getValue() + ")");

        System.out.println();
        indent += ANSI.GREY;
        indent += isLast ? "   " : "│   ";

        if (node instanceof Statement)
        {
            Object lastChild = null;

            for (Object child : ((Statement) node).getChildren())
                lastChild = child;

            for (Object child : ((Statement) node).getChildren())
                if (child instanceof List)
                    for (Object statement : (List) child)
                        printTree(statement, indent, child == lastChild);
                else
                    printTree(child, indent, child == lastChild);
        }
        else if (node instanceof Expression)
        {
            Object lastChild = null;
            for (Object child : ((Expression) node).getChildren())
                lastChild = child;

            for (Object child : ((Expression) node).getChildren())
                printTree(child, indent, child == lastChild);
        }

        System.out.print(ANSI.RESET);
    }
}
