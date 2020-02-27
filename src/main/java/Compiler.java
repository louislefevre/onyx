import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import synthesis.generation.Evaluator;

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
        TypeChecker typeChecker = new TypeChecker(parser);
        Evaluator evaluator = new Evaluator(typeChecker);

        Object output = evaluator.evaluate();

        ErrorHandler errorHandler = new ErrorHandler(lexer, parser, typeChecker, evaluator);

        if(errorHandler.errorsPresent())
            return null;

        return output;
    }
}
