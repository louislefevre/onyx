package analysis.syntax;

import analysis.lexical.Token;
import org.jetbrains.annotations.TestOnly;

public final class ParseTree
{
    public static final String RESET = "\u001B[0m";
    public static final String RED = "\u001B[31m";
    public static final String GREY = "\u001B[37m";
    public static final String BRIGHT_RED = "\u001B[91m";
    public static final String CYAN = "\u001B[36m";

    private final Expression expression;

    public ParseTree(Expression expression)
    {
        this.expression = expression;
    }

    public Expression getExpression()
    {
        this.showTree();
        return expression;
    }

    @TestOnly
    public void showTree()
    {
        printTree(this.expression, "", true);
    }

    private static void printTree(Object node, String indent, boolean isLast)
    {
        String marker = GREY;
        marker += isLast ? "└──" : "├──";

        if (node instanceof Expression)
            System.out.print(indent + marker + CYAN + ((Expression) node).getExpressionType());
        else if (node instanceof Token)
            System.out.print(indent + marker + BRIGHT_RED + ((Token) node).getTokenType());

        if (node instanceof Token && ((Token) node).getValue() != null)
            System.out.print(RED + " (" + ((Token) node).getValue() + ")");

        System.out.println();
        indent += GREY;
        indent += isLast ? "   " : "│   ";

        if (node instanceof Expression)
        {
            Object lastChild = null;
            for (Object child : ((Expression) node).getChildren())
                lastChild = child;

            for (Object child : ((Expression) node).getChildren())
                printTree(child, indent, child == lastChild);
        }

        System.out.print(RESET);
    }
}
