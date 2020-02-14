package analysis.compilation;

import analysis.binding.Binder;
import analysis.lexical.Lexer;
import analysis.syntactic.Parser;
import analysis.syntactic.SyntaxTree;

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
        Binder binder = new Binder(syntaxTree);
        Evaluator evaluator = new Evaluator(binder);

        syntaxTree.showTree(); // Testing purposes

        return evaluator.evaluate();
    }
}
