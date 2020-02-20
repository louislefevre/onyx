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

    public Object compile()
    {
        Lexer lexer = new Lexer(this.input);
        Parser parser = new Parser(lexer);
        Binder binder = new Binder(parser);
        Evaluator evaluator = new Evaluator(binder);

        SyntaxTree tree = new SyntaxTree(binder); // Testing
        tree.showTree();

        return evaluator.evaluate();
    }
}
