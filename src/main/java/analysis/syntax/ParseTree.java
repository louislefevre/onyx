package analysis.syntax;

import analysis.lexical.Token;
import org.jetbrains.annotations.TestOnly;
import util.ANSI;

public final class ParseTree
{
    private final Expression expression;

    public ParseTree(Expression expression)
    {
        this.expression = expression;
    }

    public Expression getExpression()
    {
        //this.showTree();
        return expression;
    }

    @TestOnly
    public void showTree()
    {
        printTree(this.expression, "", true);
    }

    private static void printTree(Object node, String indent, boolean isLast)
    {
        System.out.print(ANSI.GREY);

        String marker = isLast ? "└──" : "├──";

        if (node instanceof Expression)
            System.out.print(indent + marker + ((Expression) node).getExpressionType());
        else if (node instanceof Token)
            System.out.print(indent + marker + ((Token) node).getTokenType());

        if (node instanceof Token && ((Token) node).getValue() != null)
            System.out.print(" " + ((Token) node).getValue());

        System.out.println();
        indent += isLast ? "    " : "│   ";

        if (node instanceof Expression)
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
