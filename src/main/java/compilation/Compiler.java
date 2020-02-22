package compilation;

import analysis.lexical.Lexer;
import analysis.semantic.Binder;
import analysis.syntax.Parser;

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

        return evaluator.evaluate();
    }
}
