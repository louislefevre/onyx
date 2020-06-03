package analysis.semantic;

import org.junit.jupiter.api.Test;
import types.AnnotatedExpressionType;
import utilities.TestFactory;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

class TypeCheckerTest
{
    @Test
    public void parserIdentifiesLiteralExpression()
    {
        String message = "Failed to identify annotated literal expression: ";
        HashMap<String, Object> literalCollection = TestFactory.literalCollection();

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
        HashMap<String, Object> unaryCollection = TestFactory.unaryCollection();

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
        HashMap<String, Object> binaryCollection = TestFactory.binaryCollection();

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
        HashMap<String, Object> assignmentCollection = TestFactory.assignmentCollection();

        assignmentCollection.forEach((input, redundant) -> {
            AnnotatedExpressionType actual = annotatedExpressionTypeOf(input);
            AnnotatedExpressionType expected = AnnotatedExpressionType.ANNOTATED_ASSIGNMENT_EXPRESSION;
            assertEquals(expected, actual, message + input);
        });
    }

    private static AnnotatedExpressionType annotatedExpressionTypeOf(String input)
    {
        TypeChecker typeChecker = TestFactory.createTypeChecker(input);
        AnnotatedSourceStatement statement = (AnnotatedSourceStatement) typeChecker.getAnnotatedParseTree().getStatement();
        AnnotatedExpressionStatement expression = (AnnotatedExpressionStatement) statement.getStatements().get(0);

        return expression.getExpression().getExpressionType();
    }
}