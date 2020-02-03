package main.java.codeanalysis;

import java.util.List;

public final class SyntaxTree
{
    private final List<String> diagnosticsLog;
    private ExpressionSyntax expression;

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

    private void printTree(SyntaxNode node, String indent, boolean isLast)
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
            printTree(child, indent, child == lastChild);
    }

    public List<String> getDiagnosticsLog()
    {
        return this.diagnosticsLog;
    }

    public ExpressionSyntax getExpression()
    {
        return this.expression;
    }
}
