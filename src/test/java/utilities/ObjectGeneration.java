package utilities;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import source.SourceInput;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

public class ObjectGeneration
{
    private ObjectGeneration() {}

    static Lexer createLexer(String input)
    {
        SourceInput sourceInput = new SourceInput(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);
        SymbolTable symbolTable = new SymbolTable();
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
}
