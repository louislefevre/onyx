package compilation;

import analysis.semantic.Binder;
import analysis.lexical.Lexer;
import analysis.syntax.Parser;
import analysis.syntax.SyntaxTree;

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
        SyntaxTree syntaxTree = new SyntaxTree(parser);
        Binder binder = new Binder(syntaxTree);
        Evaluator evaluator = new Evaluator(binder);

        syntaxTree.showTree(); //Testing

        return evaluator.evaluate();
    }
}
