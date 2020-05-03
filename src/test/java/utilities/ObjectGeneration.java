package utilities;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import compilation.Pipeline;
import errors.ErrorHandler;
import source.SourceInput;
import source.SourceOutput;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

class ObjectGeneration
{
    private ObjectGeneration() {}

    static ErrorHandler createErrorHandler(String sourceText)
    {
        SourceInput sourceInput = createSourceInput(sourceText);
        return sourceInput.getErrorHandler();
    }

    static SourceInput createSourceInput(String input)
    {
        ErrorHandler errorHandler = new ErrorHandler();
        SymbolTable symbolTable = new SymbolTable();
        return new SourceInput(input, symbolTable, errorHandler);
    }

    static Lexer createLexer(String input)
    {
        SourceInput sourceInput = createSourceInput(input);
        return new Lexer(sourceInput);
    }

    static Parser createParser(String input)
    {
        Lexer lexer = createLexer(input);
        return new Parser(lexer);
    }

    static TypeChecker createTypeChecker(String input)
    {
        Parser parser = createParser(input);
        return new TypeChecker(parser);
    }

    static Evaluator createEvaluator(String input)
    {
        TypeChecker typeChecker = createTypeChecker(input);
        return new Evaluator(typeChecker);
    }

    static SourceOutput createSourceOutput(String input)
    {
        Evaluator evaluator = createEvaluator(input);
        return new SourceOutput(evaluator);
    }

    static Pipeline createPipeline()
    {
        return new Pipeline();
    }
}
