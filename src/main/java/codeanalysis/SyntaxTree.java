package main.java.codeanalysis;

import java.util.List;

public final class SyntaxTree
{
    private final List<String> diagnostics;
    private ExpressionSyntax root;
    private SyntaxToken endOfFileToken;

    public SyntaxTree(List<String> diagnostics, ExpressionSyntax root, SyntaxToken endOfFileToken)
    {
        this.diagnostics = diagnostics;
        this.root = root;
        this.endOfFileToken = endOfFileToken;
    }

    public List<String> getDiagnostics() { return this.diagnostics; }
    public ExpressionSyntax getRoot() { return this.root; }
    public SyntaxToken getEndOfFileToken() { return this.endOfFileToken; }

    public static SyntaxTree parse(String text)
    {
        Parser parser = new Parser(text);
        return parser.parse();
    }
}
