package compilation;

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
        ErrorHandler errorHandler = new ErrorHandler(evaluator);
        SourceOutput sourceOutput = new SourceOutput(errorHandler);

        return sourceOutput.getOutput();
    }
}
