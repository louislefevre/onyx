package analysis.syntactic;

import analysis.lexical.Token;
import analysis.lexical.Node;
import lombok.Getter;

import java.util.List;

public final class SyntaxTree
{
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_GREY = "\u001B[37m";
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
            System.out.println(ANSI_RED + diagnostic + ANSI_RESET);
    }

    public void showTree()
    {
        printTree(this.getExpression(), "", true);
    }

    private void printTree(Node node, String indent, boolean isLast)
    {
        System.out.print(ANSI_GREY);
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
        System.out.print(ANSI_RESET);
    }
}
