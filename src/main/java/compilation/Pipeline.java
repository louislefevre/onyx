package compilation;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.ParseTree;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import lombok.Getter;
import source.SourceInput;
import source.SourceOutput;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

public final class Pipeline
{
    @Getter
    private final SymbolTable symbolTable;
    private final ErrorHandler errorHandler;
    private boolean replMode;

    public Pipeline()
    {
        this.symbolTable = new SymbolTable();
        this.errorHandler = new ErrorHandler();
        this.replMode = false;
    }

    public SourceOutput compile(String sourceText)
    {
        SourceInput sourceInput = new SourceInput(sourceText, symbolTable, errorHandler, replMode);
        Lexer lexer = new Lexer(sourceInput);
        Parser parser = new Parser(lexer);
        TypeChecker typeChecker = new TypeChecker(parser);
        Evaluator evaluator = new Evaluator(typeChecker);
        SourceOutput sourceOutput = new SourceOutput(evaluator);

        return sourceOutput;
    }

    public void enableReplMode()
    {
        replMode = true;
    }

    public void printParseTree()
    {
        ParseTree.printParseTree();
    }

    public void printSymbolTable()
    {
        symbolTable.printSymbolTable();
    }
}
