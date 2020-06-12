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

/**
 * The Compiler class is used to run the full compilation process on a String of code.
 * <p>
 * It takes a String as input and runs the entire compilation process on it, producing a SourceOutput object
 * which contains the result. Each Compiler object contains its own SymbolTable instance, and initialises with
 * REPL mode set to false by default.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Setter
@Getter
public final class Compiler
{
    private final SymbolTable symbolTable;
    private boolean replMode;

    /**
     * Constructs a Compiler object initialised with an empty SymbolTable.
     * <p>
     * The Compiler always starts with REPL mode disabled.
     */
    public Compiler()
    {
        this.symbolTable = new SymbolTable();
        this.replMode = false;
    }

    /**
     * Compiles a String of code, generating a SourceOutput object containing the result.
     * <p>
     * The String passed to this method is subjected to the full compilation process:
     * - Lexical analysis (performed by the Lexer class)
     * - Syntax analysis (performed by the Parser class)
     * - Semantic analysis (performed by the TypeChecker class)
     * - Evaluation (performed by the Evaluator class)
     * The SourceOutput contains both the evaluated result and List of Errors and Exceptions
     * that may have occurred.
     *
     * @param sourceText The String of code to be compiled
     * @return A SourceOutput object containing the result
     */
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

    /**
     * Print the ParseTree generated from the latest compilation.
     * <p>
     * This may only be run after the 'compile' method has been called.
     */
    public void printParseTree()
    {
        ParseTree.print();
    }

    /**
     * Print the SymbolTable generated from the latest compilation.
     * <p>
     * This may only be run after the 'compile' method has been called.
     */
    public void printSymbolTable()
    {
        symbolTable.print();
    }
}
