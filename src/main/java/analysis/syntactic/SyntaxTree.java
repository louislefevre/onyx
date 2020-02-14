package analysis.syntactic;

import analysis.lexical.Token;
import analysis.lexical.Node;
import lombok.Getter;
import misc.ANSI;

import java.util.List;

public final class SyntaxTree
{
    @Getter private final List<String> diagnosticsLog;
    @Getter private final Expression expression;

    public SyntaxTree(Parser parser)
    {
        this.diagnosticsLog = parser.getDiagnosticsLog();
        this.expression = parser.getExpression();
    }

    public void showTree()
    {
        printTree(this.getExpression(), "", true);
    }

    private void printTree(Node node, String indent, boolean isLast)
    {
        System.out.print(ANSI.GREY);
        String marker = isLast ? "└──" : "├──";

        System.out.print(indent + marker + node.getType());

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
