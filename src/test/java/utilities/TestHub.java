package utilities;

import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import identifiers.TokenType;
import synthesis.generation.Evaluator;

import java.util.HashMap;

public class TestHub
{
    private TestHub() {}

    public static Lexer createLexer(String input)
    {
        return ObjectGeneration.createLexer(input);
    }

    public static Parser createParser(String input)
    {
        return ObjectGeneration.createParser(input);
    }

    public static TypeChecker createTypeChecker(String input)
    {
        return ObjectGeneration.createTypeChecker(input);
    }

    public static Evaluator createEvaluator(String input)
    {
        return ObjectGeneration.createEvaluator(input);
    }

    public static HashMap<String, Object> literalCollection()
    {
        HashMap<String, Object> data = new HashMap<>();
        data.putAll(MapGeneration.integerCollection());
        data.putAll(MapGeneration.doubleCollection());
        data.putAll(MapGeneration.booleanCollection());
        data.putAll(MapGeneration.stringCollection());

        return data;
    }

    public static HashMap<String, Object> unaryCollection()
    {
        HashMap<String, Object> unarys = new HashMap<>();
        unarys.putAll(MapGeneration.unaryIntegerCollection());
        unarys.putAll(MapGeneration.unaryDoubleCollection());
        unarys.putAll(MapGeneration.unaryBooleanCollection());

        return unarys;
    }

    public static HashMap<String, Object> binaryCollection()
    {
        HashMap<String, Object> binaries = new HashMap<>();
        binaries.putAll(MapGeneration.binaryIntegerCollection());
        binaries.putAll(MapGeneration.binaryDoubleCollection());
        binaries.putAll(MapGeneration.binaryBooleanCollection());
        binaries.putAll(MapGeneration.binaryStringCollection());

        return binaries;
    }

    public static HashMap<String, Object> assignmentCollection()
    {
        return MapGeneration.assignmentCollection();
    }

    public static HashMap<String, Object> identifierCollection()
    {
        return MapGeneration.identifierCollection();
    }

    public static HashMap<String, TokenType> tokenTypeCollection()
    {
        return MapGeneration.tokenTypeCollection();
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
