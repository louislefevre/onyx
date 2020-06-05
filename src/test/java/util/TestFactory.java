package util;

import compilation.Compiler;
import compilation.analysis.lexical.Lexer;
import compilation.analysis.semantic.TypeChecker;
import compilation.analysis.syntax.Parser;
import compilation.generation.Evaluator;
import errors.Error;
import types.TokenType;

import java.util.HashMap;

public final class TestFactory
{
    public static Lexer createLexer(String input)
    {
        return CompilerFactory.createLexer(input);
    }

    public static Parser createParser(String input, boolean replMode)
    {
        return CompilerFactory.createParser(input, replMode);
    }

    public static TypeChecker createTypeChecker(String input, boolean replMode)
    {
        return CompilerFactory.createTypeChecker(input, replMode);
    }

    public static Evaluator createEvaluator(String input, boolean replMode)
    {
        return CompilerFactory.createEvaluator(input, replMode);
    }

    public static Compiler createCompiler(boolean replMode)
    {
        return CompilerFactory.createCompiler(replMode);
    }

    public static HashMap<String, Object> literalCollection()
    {
        HashMap<String, Object> data = new HashMap<>();
        data.putAll(DataFactory.integerCollection());
        data.putAll(DataFactory.doubleCollection());
        data.putAll(DataFactory.booleanCollection());
        data.putAll(DataFactory.stringCollection());

        return data;
    }

    public static HashMap<String, Object> unaryCollection()
    {
        HashMap<String, Object> unarys = new HashMap<>();
        unarys.putAll(DataFactory.unaryIntegerCollection());
        unarys.putAll(DataFactory.unaryDoubleCollection());
        unarys.putAll(DataFactory.unaryBooleanCollection());

        return unarys;
    }

    public static HashMap<String, Object> binaryCollection()
    {
        HashMap<String, Object> binaries = new HashMap<>();
        binaries.putAll(DataFactory.binaryIntegerCollection());
        binaries.putAll(DataFactory.binaryDoubleCollection());
        binaries.putAll(DataFactory.binaryBooleanCollection());
        binaries.putAll(DataFactory.binaryStringCollection());

        return binaries;
    }

    public static HashMap<String, Object> identifierCollection()
    {
        return DataFactory.identifierCollection();
    }

    public static HashMap<String, Object> assignmentCollection()
    {
        return DataFactory.assignmentCollection();
    }

    public static HashMap<String, Object> assignmentOperatorsCollection()
    {
        return DataFactory.assignmentOperatorsCollection();
    }

    public static HashMap<String, Object> reassignmentCollection()
    {
        return DataFactory.reassignmentCollection();
    }

    public static HashMap<String, TokenType> tokenTypeCollection()
    {
        return DataFactory.tokenTypeCollection();
    }

    public static HashMap<String, Error> lexicalErrorCollection()
    {
        return DataFactory.lexicalErrorCollection();
    }

    public static HashMap<String, Error> syntaxErrorCollection()
    {
        return DataFactory.syntaxErrorCollection();
    }

    public static HashMap<String, Error> semanticErrorCollection()
    {
        return DataFactory.semanticErrorCollection();
    }

    public static HashMap<String, Object> parenthesizedCollection()
    {
        HashMap<String, Object> allCollections = allCollections();
        HashMap<String, Object> parenthesizes = new HashMap<>();
        allCollections.forEach((k, v) -> parenthesizes.put("(" + k + ")", v));

        return parenthesizes;
    }

    public static HashMap<String, Object> allCollections()
    {
        HashMap<String, Object> allCollections = new HashMap<>();
        allCollections.putAll(literalCollection());
        allCollections.putAll(unaryCollection());
        allCollections.putAll(binaryCollection());
        allCollections.putAll(assignmentCollection());

        return allCollections;
    }
}
