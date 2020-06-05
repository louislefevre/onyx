package compilation.generation;

import exceptions.Exception;
import org.junit.jupiter.api.Test;
import util.TestFactory;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

class EvaluatorTest
{
    @Test
    public void evaluatorEvaluatesLiterals()
    {
        String message = "Failed to evaluate literal expression: ";
        HashMap<String, Object> literalCollection = TestFactory.literalCollection();

        literalCollection.forEach((input, expected) -> {
            Object actual = evaluationOf(input);
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void evaluatorEvaluatesUnary()
    {
        String message = "Failed to evaluate unary expression: ";
        HashMap<String, Object> unaryCollection = TestFactory.unaryCollection();

        unaryCollection.forEach((input, expected) -> {
            Object actual = evaluationOf(input);
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void evaluatorEvaluatesBinaries()
    {
        String message = "Failed to evaluate binary expression: ";
        HashMap<String, Object> binaryCollection = TestFactory.binaryCollection();

        binaryCollection.forEach((input, expected) -> {
            Object actual = evaluationOf(input);
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void evaluatorEvaluatesParameters()
    {
        String message = "Failed to evaluate parenthesized expression: ";
        HashMap<String, Object> parenthesizedCollection = TestFactory.parenthesizedCollection();

        parenthesizedCollection.forEach((input, expected) -> {
            Object actual = evaluationOf(input);
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void evaluatorEvaluatesAssignment()
    {
        String message = "Failed to evaluate assignment expression: ";
        HashMap<String, Object> assignmentCollection = TestFactory.assignmentCollection();

        assignmentCollection.forEach((input, expected) -> {
            Object actual = evaluationOf(input);
            assertEquals(expected, actual, message + input);
        });
    }

    private static Object evaluationOf(String input)
    {
        Evaluator evaluator = TestFactory.createEvaluator(input, true);
        try
        {
            Object[] result = evaluator.getEvaluation();
            return result[0];
        }
        catch (Exception exception)
        {
            return exception.getMessage();
        }
    }
}