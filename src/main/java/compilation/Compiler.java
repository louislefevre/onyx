package compilation;

import analysis.semantic.Binder;
import analysis.lexical.Lexer;
import analysis.syntax.Parser;
import analysis.syntax.ParseTree;

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
        ParseTree parseTree = new ParseTree(parser);
        Binder binder = new Binder(parseTree);
        Evaluator evaluator = new Evaluator(binder);

        parseTree.showTree(); //Testing

        return evaluator.evaluate();
    }
}
