package analysis.syntax;

import analysis.lexical.Node;
import analysis.lexical.Token;
import misc.ANSI;

public final class SyntaxTree
{
    private final Expression expression;

    public SyntaxTree(Parser parser)
    {
        this.expression = parser.getExpression();
    }

    public Expression getExpression()
    {
        return expression;
    }

    public void showTree()
    {
        printTree(this.expression, "", true);
    }

    private void printTree(Node node, String indent, boolean isLast)
    {
        System.out.print(ANSI.GREY);
        String marker = isLast ? "└──" : "├──";

        System.out.print(indent + marker + node.getTokenType());

        if(node instanceof Token && ((Token) node).getValue() != null)
            System.out.print(" " + ((Token) node).getValue());

        System.out.println();
        indent += isLast ? "    " : "│   ";

        Node lastChild = null;
        for(Node child : node.getChildren())
            lastChild = child;

        for(Node child : node.getChildren())
            printTree(child, indent, child == lastChild);
        System.out.print(ANSI.RESET);
    }
}
