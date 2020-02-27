import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import synthesis.generation.Evaluator;
import synthesis.generation.SourceOutput;

public final class Compiler
{
    public Compiler() { }

    public Object compile(String input)
    {
        Lexer lexer = new Lexer(input);
        Parser parser = new Parser(lexer);
        TypeChecker typeChecker = new TypeChecker(parser);
        Evaluator evaluator = new Evaluator(typeChecker);
        SourceOutput sourceOutput = new SourceOutput(evaluator);

        ErrorHandler errorHandler = new ErrorHandler(lexer, parser, typeChecker, evaluator);

        if(errorHandler.errorsPresent())
            return null;
        return sourceOutput.getOutput();
    }
}
