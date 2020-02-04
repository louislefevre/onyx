package main.java.analysis;

public class Compiler
{
    private String input;

    public Compiler(String input)
    {
        this.input = input;
    }

    public int compile()
    {
        Lexer lexer = new Lexer(this.input);
        Parser parser = new Parser(lexer);
        SyntaxTree syntaxTree = new SyntaxTree(parser);
        Evaluator evaluator = new Evaluator(syntaxTree);

        syntaxTree.showTree(); // Testing purposes

        return evaluator.evaluate();
    }
}
