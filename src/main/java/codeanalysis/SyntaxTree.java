package main.java.codeanalysis;

import java.util.List;

public final class SyntaxTree
{
    private final List<String> diagnostics;
    private ExpressionSyntax root;
    private SyntaxToken endOfFileToken;

    public SyntaxTree(Parser parser)
    {
        this.diagnostics = parser.getDiagnostics();
        this.root = parser.getExpression();
        this.endOfFileToken = parser.getEndOfFileToken();
    }

    public List<String> getDiagnostics() { return this.diagnostics; }
    public ExpressionSyntax getRoot() { return this.root; }
    public SyntaxToken getEndOfFileToken() { return this.endOfFileToken; }

    public void showDiagnostics()
    {
        for (String diagnostic : this.getDiagnostics())
            System.out.println(diagnostic);
    }

    public void showTree()
    {
        printTree(this.getRoot(), "", true);
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
}
