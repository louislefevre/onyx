package analysis.semantic;

import source.SourceInput;
import analysis.lexical.Lexer;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import org.jetbrains.annotations.NotNull;
import symbols.SymbolTable;

class TypeCheckerTest
{
    @NotNull
    private static TypeChecker createTypeChecker(String input)
    {
        SymbolTable symbolTable = new SymbolTable();
        SourceInput sourceInput = new SourceInput(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);
        Lexer lexer = new Lexer(sourceInput, errorHandler);
        Parser parser = new Parser(lexer, errorHandler);
        return new TypeChecker(parser, errorHandler, symbolTable);
    }
}