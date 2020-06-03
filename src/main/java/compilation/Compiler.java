package compilation;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.ParseTree;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import generation.Evaluator;
import lombok.Getter;
import lombok.Setter;
import source.SourceInput;
import source.SourceOutput;
import symbols.SymbolTable;

import java.util.stream.Stream;

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

    public SourceOutput compileInput(String input)
    {
        if(replMode)
            return compileSingleLine(input);
        return compileMultiLine(input);
    }

    private SourceOutput compileSingleLine(String input)
    {
        return compile(input);
    }

    private SourceOutput compileMultiLine(String input)
    {
        input += System.lineSeparator(); // Adds extra line separator at end to avoid collision with EOF
        input = input.replaceAll("\011", ""); // Ignores horizontal tabs, breaks line separators otherwise
        Stream<String> lines = input.lines(); // Splits each line up to be run individually

        StringBuilder builder = new StringBuilder();
        lines.forEach(line -> {
            builder.append(line);
            builder.append(System.lineSeparator());
            compile(builder.toString());
        });

        return compile(input);
    }

    private SourceOutput compile(String sourceText)
    {
        SourceInput sourceInput = new SourceInput(sourceText);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);

        Lexer lexer = new Lexer(sourceInput, errorHandler);
        Parser parser = new Parser(lexer, errorHandler, replMode);
        TypeChecker typeChecker = new TypeChecker(parser, errorHandler, symbolTable);
        Evaluator evaluator = new Evaluator(typeChecker, errorHandler, symbolTable);
        SourceOutput sourceOutput = new SourceOutput(evaluator, errorHandler);

        return sourceOutput;
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
