package util;

import compilation.Compiler;
import compilation.analysis.lexical.Lexer;
import compilation.analysis.semantic.TypeChecker;
import compilation.analysis.syntax.Parser;
import compilation.generation.Evaluator;
import errors.ErrorHandler;
import source.SourceInput;
import symbols.SymbolTable;

final class CompilerFactory
{
    static Lexer createLexer(String input)
    {
        SourceInput sourceInput = new SourceInput(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);
        return new Lexer(sourceInput, errorHandler);
    }

    static Parser createParser(String input, boolean replMode)
    {
        SourceInput sourceInput = new SourceInput(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);
        Lexer lexer = new Lexer(sourceInput, errorHandler);
        return new Parser(lexer, errorHandler, replMode);
    }

    static TypeChecker createTypeChecker(String input, boolean replMode)
    {
        SourceInput sourceInput = new SourceInput(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);
        Lexer lexer = new Lexer(sourceInput, errorHandler);
        Parser parser = new Parser(lexer, errorHandler, replMode);
        SymbolTable symbolTable = new SymbolTable();
        return new TypeChecker(parser, errorHandler, symbolTable);
    }

    static Evaluator createEvaluator(String input, boolean replMode)
    {
        SourceInput sourceInput = new SourceInput(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);
        Lexer lexer = new Lexer(sourceInput, errorHandler);
        Parser parser = new Parser(lexer, errorHandler, replMode);
        SymbolTable symbolTable = new SymbolTable();
        TypeChecker typeChecker = new TypeChecker(parser, errorHandler, symbolTable);
        return new Evaluator(typeChecker, symbolTable, replMode);
    }

    static Compiler createCompiler(boolean replMode)
    {
        Compiler compiler = new Compiler();
        compiler.setReplMode(replMode);
        return compiler;
    }
}
