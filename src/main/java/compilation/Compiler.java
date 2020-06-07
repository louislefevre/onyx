package compilation;

import compilation.analysis.lexical.Lexer;
import compilation.analysis.semantic.TypeChecker;
import compilation.analysis.syntax.ParseTree;
import compilation.analysis.syntax.Parser;
import compilation.generation.Evaluator;
import errors.ErrorHandler;
import lombok.Getter;
import lombok.Setter;
import source.SourceInput;
import source.SourceOutput;
import symbols.SymbolTable;

@Setter
@Getter
public final class Compiler
{
    private final SymbolTable symbolTable;
    private boolean replMode;

    public Compiler()
    {
        this.symbolTable = new SymbolTable();
        this.replMode = false;
    }

    public SourceOutput compile(String sourceText)
    {
        SourceInput sourceInput = new SourceInput(sourceText);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);

        Lexer lexer = new Lexer(sourceInput, errorHandler);
        Parser parser = new Parser(lexer, errorHandler, replMode);
        TypeChecker typeChecker = new TypeChecker(parser, errorHandler, symbolTable);
        Evaluator evaluator = new Evaluator(typeChecker, symbolTable, replMode);
        SourceOutput sourceOutput = new SourceOutput(evaluator, errorHandler);

        return sourceOutput;
    }

    public void printParseTree()
    {
        ParseTree.print();
    }

    public void printSymbolTable()
    {
        symbolTable.print();
    }
}
