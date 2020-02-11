package analysis;

import analysis.lexical.Lexer;
import analysis.syntactic.Parser;
import analysis.syntactic.SyntaxTree;
import analysis.syntactic.Evaluator;

public final class Compiler
{
    private final String input;

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
