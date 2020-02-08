package analysis.syntactic;

import analysis.lexical.Token;
import analysis.lexical.Node;
import lombok.Getter;

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

    public void showDiagnostics()
    {
        for (String diagnostic : this.getDiagnosticsLog())
            System.out.println(diagnostic);
    }

    public void showTree()
    {
        printTree(this.getExpression(), "", true);
    }

    private void printTree(Node node, String indent, boolean isLast)
    {
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
    }
}
