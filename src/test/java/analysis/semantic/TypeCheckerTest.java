package analysis.semantic;

import identifiers.AnnotatedExpressionType;
import org.junit.jupiter.api.Test;
import utilities.TestHub;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

class TypeCheckerTest
{
    @Test
    public void parserIdentifiesLiteralExpression()
    {
        String message = "Failed to identify annotated literal expression: ";
        HashMap<String, Object> literalCollection = TestHub.literalCollection();

        literalCollection.forEach((input, redundant) -> {
            AnnotatedExpressionType actual = annotatedExpressionTypeOf(input);
            AnnotatedExpressionType expected = AnnotatedExpressionType.ANNOTATED_LITERAL_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void parserIdentifiesUnaryExpression()
    {
        String message = "Failed to identify annotated unary expression: ";
        HashMap<String, Object> unaryCollection = TestHub.unaryCollection();

        unaryCollection.forEach((input, redundant) -> {
            AnnotatedExpressionType actual = annotatedExpressionTypeOf(input);
            AnnotatedExpressionType expected = AnnotatedExpressionType.ANNOTATED_UNARY_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void parserIdentifiesBinaryExpression()
    {
        String message = "Failed to identify annotated binary expression: ";
        HashMap<String, Object> binaryCollection = TestHub.binaryCollection();

        binaryCollection.forEach((input, redundant) -> {
            AnnotatedExpressionType actual = annotatedExpressionTypeOf(input);
            AnnotatedExpressionType expected = AnnotatedExpressionType.ANNOTATED_BINARY_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void parserIdentifiesAssignmentExpression()
    {
        String message = "Failed to identify annotated assignment expression: ";
        HashMap<String, Object> assignmentCollection = TestHub.assignmentCollection();

        assignmentCollection.forEach((input, redundant) -> {
            AnnotatedExpressionType actual = annotatedExpressionTypeOf(input);
            AnnotatedExpressionType expected = AnnotatedExpressionType.ANNOTATED_ASSIGNMENT_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    private static AnnotatedExpressionType annotatedExpressionTypeOf(String input)
    {
        TypeChecker typeChecker = TestHub.createTypeChecker(input);
        AnnotatedExpressionStatement expression =
                (AnnotatedExpressionStatement) typeChecker.getAnnotatedParseTree().getAnnotatedStatement();
        return expression.getAnnotatedExpression().getAnnotatedExpressionType();
    }
}