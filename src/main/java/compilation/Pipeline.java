package compilation;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import source.SourceInput;
import source.SourceOutput;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

public final class Pipeline
{
    private final SymbolTable symbolTable;

    public Pipeline()
    {
        this.symbolTable = new SymbolTable();
    }

    public SourceOutput compile(String input)
    {
        SourceInput sourceInput = new SourceInput(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);

        Lexer lexer = new Lexer(sourceInput, errorHandler, this.symbolTable);
        Parser parser = new Parser(lexer);
        TypeChecker typeChecker = new TypeChecker(parser);
        Evaluator evaluator = new Evaluator(typeChecker);
        SourceOutput sourceOutput = new SourceOutput(evaluator);

        return sourceOutput;
    }
}
