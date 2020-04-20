package compilation;

import analysis.SourceText;
import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import org.jetbrains.annotations.NotNull;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;
import synthesis.generation.SourceOutput;

public final class Compiler
{
    private final SymbolTable symbolTable;

    public Compiler()
    {
        this.symbolTable = new SymbolTable();
    }

    @NotNull
    public SourceOutput compile(String input)
    {
        SymbolTable symbolTable = this.symbolTable;

        SourceText sourceText = new SourceText(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceText);

        Lexer lexer = new Lexer(sourceText, errorHandler);
        Parser parser = new Parser(lexer, errorHandler);
        TypeChecker typeChecker = new TypeChecker(parser, errorHandler, symbolTable);
        Evaluator evaluator = new Evaluator(typeChecker, errorHandler, symbolTable);
        SourceOutput sourceOutput = new SourceOutput(evaluator, errorHandler);

        return sourceOutput;
    }
}
