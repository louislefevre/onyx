package analysis.syntax;

import identifiers.ExpressionType;
import org.junit.jupiter.api.Test;
import utilities.TestHub;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ParserTest
{
    @Test
    public void parserIdentifiesLiteralExpression()
    {
        String message = "Failed to identify literal expression: ";
        HashMap<String, Object> literalCollection = TestHub.literalCollection();

        literalCollection.forEach((input, redundant) -> {
            ExpressionType actual = expressionTypeOf(input);
            ExpressionType expected = ExpressionType.LITERAL_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void parserIdentifiesUnaryExpression()
    {
        String message = "Failed to identify unary expression: ";
        HashMap<String, Object> unaryCollection = TestHub.unaryCollection();

        unaryCollection.forEach((input, redundant) -> {
            ExpressionType actual = expressionTypeOf(input);
            ExpressionType expected = ExpressionType.UNARY_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void parserIdentifiesBinaryExpression()
    {
        String message = "Failed to identify binary expression: ";
        HashMap<String, Object> binaryCollection = TestHub.binaryCollection();

        binaryCollection.forEach((input, redundant) -> {
            ExpressionType actual = expressionTypeOf(input);
            ExpressionType expected = ExpressionType.BINARY_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void parserIdentifiesIdentifierExpression()
    {
        String message = "Failed to identify identifier expression: ";
        HashMap<String, Object> identifierCollection = TestHub.identifierCollection();

        identifierCollection.forEach((input, redundant) -> {
            ExpressionType actual = expressionTypeOf(input);
            ExpressionType expected = ExpressionType.IDENTIFIER_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void parserIdentifiesAssignmentExpression()
    {
        String message = "Failed to identify assignment expression: ";
        HashMap<String, Object> assignmentCollection = TestHub.assignmentCollection();

        assignmentCollection.forEach((input, redundant) -> {
            ExpressionType actual = expressionTypeOf(input);
            ExpressionType expected = ExpressionType.ASSIGNMENT_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void parserIdentifiesParenthesizedExpression()
    {
        String message = "Failed to identify parenthesized expression: ";
        HashMap<String, Object> parenthesizedCollection = TestHub.parenthesizedCollection();

        parenthesizedCollection.forEach((input, redundant) -> {
            ExpressionType actual = expressionTypeOf(input);
            ExpressionType expected = ExpressionType.PARENTHESIZED_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    private static ExpressionType expressionTypeOf(String input)
    {
        Parser parser = TestHub.createParser(input);
        ExpressionStatement expression = (ExpressionStatement) parser.getParseTree().getStatement();
        return expression.getExpression().getExpressionType();
    }
}
