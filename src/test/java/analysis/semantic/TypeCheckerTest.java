package analysis.semantic;

import analysis.lexical.Lexer;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import org.jetbrains.annotations.NotNull;
import symbols.SymbolTable;

import static org.junit.jupiter.api.Assertions.*;

class TypeCheckerTest
{
    @NotNull
    private static TypeChecker createTypeChecker(String input)
    {
        SymbolTable symbolTable = new SymbolTable();
        ErrorHandler errorHandler = new ErrorHandler(input);
        Lexer lexer = new Lexer(input, errorHandler);
        Parser parser = new Parser(lexer, errorHandler);
        return new TypeChecker(parser, errorHandler, symbolTable);
    }
}