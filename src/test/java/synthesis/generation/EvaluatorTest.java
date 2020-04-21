package synthesis.generation;

import source.SourceInput;
import analysis.lexical.Lexer;
import analysis.semantic.TypeChecker;
import analysis.syntax.Parser;
import errors.ErrorHandler;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;
import symbols.SymbolTable;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

class EvaluatorTest
{
    @Test
    public void evaluatorEvaluatesLiterals()
    {
        String message = "Failed to evaluate literal expression: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("0", 0);
        data.put("1", 1);
        data.put("+1", 1);
        data.put("-1", -1);
        data.put("true", true);
        data.put("false", false);
        data.put("!true", false);
        data.put("!false", true);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesAddition()
    {
        String message = "Failed to evaluate addition expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("0 + 0", 0);
        data.put("10 + 5", 15);
        data.put("+10 + +5", 15);
        data.put("-10 + -5", -15);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesSubtraction()
    {
        String message = "Failed to evaluate subtraction expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("0 - 0", 0);
        data.put("10 - 5", 5);
        data.put("+10 - +5", 5);
        data.put("-10 - -5", -5);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesMultiplication()
    {
        String message = "Failed to evaluate multiplication expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("0 * 0", 0);
        data.put("10 * 5", 50);
        data.put("+10 * +5", 50);
        data.put("-10 * -5", 50);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesDivision()
    {
        String message = "Failed to evaluate division expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("0/0", 0);
        data.put("10/5", 2);
        data.put("+10/+5", 2);
        data.put("-10/-5", 2);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesPowers()
    {
        String message = "Failed to evaluate powers expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("0^0", 1);
        data.put("10^5", 100000);
        data.put("+10^+5", 100000);
        data.put("-10^-5", 0);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesModulo()
    {
        String message = "Failed to evaluate modulo expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("0 % 0", 0);
        data.put("10 % 4", 2);
        data.put("+10 % +4", 2);
        data.put("-10 % -4", -2);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesParameters()
    {
        String message = "Failed to evaluate parameters expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("0 + (0)", 0);
        data.put("10 + (5)", 15);
        data.put("+10 + (+5)", 15);
        data.put("-10 + (-5)", -15);
        data.put("(2+3) * 10", 50);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesConditionals()
    {
        String message = "Failed to evaluate conditionals expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("true AND true", true);
        data.put("false AND false", false);
        data.put("true AND false", false);
        data.put("false AND true", false);
        data.put("true OR true", true);
        data.put("false OR false", false);
        data.put("true OR false", true);
        data.put("false OR true", true);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesEquals()
    {
        String message = "Failed to evaluate equals expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("true == true", true);
        data.put("false == false", true);
        data.put("true == false", false);
        data.put("false == true", false);
        data.put("10 == 10", true);
        data.put("10 == 5", false);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesNotEquals()
    {
        String message = "Failed to evaluate not equals expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("true == !true", false);
        data.put("false == !false", false);
        data.put("true == !false", true);
        data.put("false == !true", true);
        data.put("true != true", false);
        data.put("false != false", false);
        data.put("true != false", true);
        data.put("false != true", true);
        data.put("10 != 10", false);
        data.put("10 != 5", true);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesGreaterThan()
    {
        String message = "Failed to evaluate greater than expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("0 > 0", false);
        data.put("10 > 5", true);
        data.put("+10 > +5", true);
        data.put("-10 > -5", false);
        data.put("0 >= 0", true);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesLessThan()
    {
        String message = "Failed to evaluate less than expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("0 < 0", false);
        data.put("10 < 5", false);
        data.put("+10 < +5", false);
        data.put("-10 < -5", true);
        data.put("0 <= 0", true);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    @Test
    public void evaluatorEvaluatesAssignment()
    {
        String message = "Failed to evaluate assignment expresion: ";
        HashMap<String, Object> data = new HashMap<>();

        data.put("a=0", 0);
        data.put("a=10", 10);
        data.put("a=10*5", 50);
        data.put("a=true", true);

        data.forEach((input, expected) ->
                             assertEquals(expected, createEvaluator(input).getEvaluation(), message + input));
    }

    private static @NotNull Evaluator createEvaluator(String input)
    {
        SymbolTable symbolTable = new SymbolTable();
        SourceInput sourceInput = new SourceInput(input);
        ErrorHandler errorHandler = new ErrorHandler(sourceInput);
        Lexer lexer = new Lexer(sourceInput, errorHandler);
        Parser parser = new Parser(lexer, errorHandler);
        TypeChecker typeChecker = new TypeChecker(parser, errorHandler, symbolTable);
        return new Evaluator(typeChecker, errorHandler, symbolTable);
    }
}