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

    static SymbolTable createSymbolTable()
    {
        return new SymbolTable();
    }

    static SourceInput createSourceInput(String input)
    {
        return new SourceInput(input);
    }

    static ErrorHandler createErrorHandler(String input)
    {
        SourceInput sourceInput = new SourceInput(input);
        return new ErrorHandler(sourceInput);
    }

    static ErrorHandler createErrorHandler(SourceInput sourceInput)
    {
        return new ErrorHandler(sourceInput);
    }

    static Lexer createLexer(String input)
    {
        SourceInput sourceInput = createSourceInput(input);
        ErrorHandler errorHandler = createErrorHandler(sourceInput);
        SymbolTable symbolTable = createSymbolTable();
        return new Lexer(sourceInput, errorHandler, symbolTable);
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
