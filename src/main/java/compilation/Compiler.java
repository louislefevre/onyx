package compilation;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import source.SourceInput;
import source.SourceOutput;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

public final class Compiler
{
    private final SymbolTable symbolTable;

    public Compiler()
    {
        this.symbolTable = new SymbolTable();
    }

    public SourceOutput compile(String input)
    {
        SourceInput sourceInput = new SourceInput(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);

        Lexer lexer = new Lexer(sourceInput, errorHandler);
        Parser parser = new Parser(lexer, errorHandler);
        TypeChecker typeChecker = new TypeChecker(parser, errorHandler, this.symbolTable);
        Evaluator evaluator = new Evaluator(typeChecker, errorHandler, this.symbolTable);
        SourceOutput sourceOutput = new SourceOutput(evaluator, errorHandler);

        return sourceOutput;
    }
}
