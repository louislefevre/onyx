package compilation;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;
import synthesis.generation.SourceOutput;

public final class Compiler
{
    private final SymbolTable symbolTable;

    public Compiler(SymbolTable symbolTable)
    {
        this.symbolTable = symbolTable;
    }

    public SourceOutput compile(String input)
    {
        SymbolTable symbolTable = this.symbolTable;
        ErrorHandler errorHandler = new ErrorHandler(input);

        Lexer lexer = new Lexer(input, errorHandler);
        Parser parser = new Parser(lexer, errorHandler);
        TypeChecker typeChecker = new TypeChecker(parser, errorHandler, symbolTable);
        Evaluator evaluator = new Evaluator(typeChecker, errorHandler);
        SourceOutput sourceOutput = new SourceOutput(evaluator, errorHandler);

        return sourceOutput;
    }
}
