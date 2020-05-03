package compilation;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.ParseTree;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import source.SourceInput;
import source.SourceOutput;
import symbols.SymbolTable;
import synthesis.generation.Evaluator;

public final class Pipeline
{
    private final SymbolTable symbolTable;
    private final ErrorHandler errorHandler;

    public Pipeline()
    {
        this.symbolTable = new SymbolTable();
        this.errorHandler = new ErrorHandler();
    }

    public SourceOutput compile(String sourceText)
    {
        SourceInput sourceInput = new SourceInput(sourceText, this.symbolTable, this.errorHandler);
        Lexer lexer = new Lexer(sourceInput);
        Parser parser = new Parser(lexer);
        TypeChecker typeChecker = new TypeChecker(parser);
        Evaluator evaluator = new Evaluator(typeChecker);
        SourceOutput sourceOutput = new SourceOutput(evaluator);

        ParseTree.printParseTree();
        symbolTable.printSymbolTable();

        return sourceOutput;
    }
}
