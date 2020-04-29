package utilities;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import source.SourceInput;
import source.SourceOutput;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

public class TestHub
{
    private TestHub() { }

    public static Lexer createLexer(String input)
    {
        SourceInput sourceInput = new SourceInput(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);
        SymbolTable symbolTable = new SymbolTable();
        return new Lexer(sourceInput, errorHandler, symbolTable);
    }

    public static Parser createParser(String input)
    {
        Lexer lexer = createLexer(input);
        return new Parser(lexer);
    }

    public static TypeChecker createTypeChecker(String input)
    {
        Parser parser = createParser(input);
        return new TypeChecker(parser);
    }

    public static Evaluator createEvaluator(String input)
    {
        TypeChecker typeChecker = createTypeChecker(input);
        return new Evaluator(typeChecker);
    }

    public static SourceOutput createSourceOutput(String input)
    {
        Evaluator evaluator = createEvaluator(input);
        return new SourceOutput(evaluator);
    }
}

/*
* TODO: To ensure the testing inputs are consistent, create methods for storing and returning an
*       array of different inputs. That way by putting a new input into this array, its used in every
*       single test class.
*/