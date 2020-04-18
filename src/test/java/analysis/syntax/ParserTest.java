package analysis.syntax;

import analysis.lexical.Lexer;
import analysis.lexical.Syntax;
import errors.ErrorHandler;
import identifiers.TokenType;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ParserTest
{
    @Test
    public void parserIdentifiesNumericalLiteralExpression()
    {
        TokenType expectedExpression = TokenType.LITERAL_EXPRESSION_TOKEN;
        String message = "Failed to identify numerical literal expression: ";

        int valueLimit = 1000;

        for (int i = 0; i <= valueLimit; i++)
            assertEquals(expectedExpression, expressionTypeOf(String.valueOf(i)), message + i);
    }

    @Test
    public void parserIdentifiesConditionalLiteralExpression()
    {
        TokenType expectedExpression = TokenType.LITERAL_EXPRESSION_TOKEN;
        String message = "Failed to identify numerical literal expression: ";

        String[] keywords = {Syntax.TRUE.getSyntax(), Syntax.FALSE.getSyntax()};

        for (String keyword : keywords)
            assertEquals(expectedExpression, expressionTypeOf(keyword), message + keyword);
    }

    @Test
    public void parserIdentifiesNumericalUnaryExpression()
    {
        TokenType expectedExpression = TokenType.UNARY_EXPRESSION_TOKEN;
        String message = "Failed to identify numerical unary expression: ";

        int valueLimit = 1000;
        String[] unaryOperators = {Syntax.PLUS.getSyntax(), Syntax.MINUS.getSyntax(), Syntax.NOT.getSyntax()};

        for (int i = 0, j = 0; i <= valueLimit; i++, j++)
        {
            for (String op : unaryOperators)
            {
                String expression = op + i;
                assertEquals(expectedExpression, expressionTypeOf(expression), message + expression);
            }

            j += i; // Varies numbers (can be adapted)
        }
    }

    @Test
    public void parserIdentifiesConditionalUnaryExpression()
    {
        TokenType expectedExpression = TokenType.UNARY_EXPRESSION_TOKEN;
        String message = "Failed to identify conditional unary expression: ";

        String[] keywords = {Syntax.TRUE.getSyntax(), Syntax.FALSE.getSyntax()};
        String[] unaryOperators = {Syntax.NOT.getSyntax()};

        for (String keyword : keywords)
        {
            for (String op : unaryOperators)
            {
                String expression = op + keyword;
                assertEquals(expectedExpression, expressionTypeOf(expression), message + expression);
            }
        }
    }

    @Test
    public void parserIdentifiesNumericalBinaryExpression()
    {
        TokenType expectedExpression = TokenType.BINARY_EXPRESSION_TOKEN;
        String message = "Failed to identify numerical binary expression: ";

        int valueLimit = 100;
        String[] binaryOperators = {Syntax.PLUS.getSyntax(), Syntax.MINUS.getSyntax(), Syntax.STAR.getSyntax(),
                                    Syntax.SLASH.getSyntax(), Syntax.CARET.getSyntax(), Syntax.PERCENT.getSyntax(),
                                    Syntax.EQUALS_EQUALS.getSyntax(), Syntax.NOT_EQUALS.getSyntax(),
                                    Syntax.GREATER.getSyntax(), Syntax.LESS.getSyntax(),
                                    Syntax.GREATER_EQUALS.getSyntax(), Syntax.LESS_EQUALS.getSyntax()};
        String[] unaryOperators = {"",
                                   Syntax.PLUS.getSyntax(), Syntax.MINUS.getSyntax(),
                                   Syntax.NOT.getSyntax(), Syntax.OPEN_PARENTHESIS.getSyntax()};

        for (int i = 0, j = 0; i <= valueLimit; i++, j++)
        {
            for (String unaryOp1 : unaryOperators)
            {
                String term1 = appendUnaryOperator(unaryOp1, i);
                for (String unaryOp2 : unaryOperators)
                {
                    String term2 = appendUnaryOperator(unaryOp2, i);
                    for (String op : binaryOperators)
                    {
                        String expression = term1 + op + term2;
                        assertEquals(expectedExpression, expressionTypeOf(expression), message + expression);
                    }
                }
            }
            j += i; // Varies numbers (can be adapted)
        }
    }

    @Test
    public void parserIdentifiesConditionalBinaryExpression()
    {
        TokenType expectedExpression = TokenType.BINARY_EXPRESSION_TOKEN;
        String message = "Failed to identify conditional binary expression: ";

        String[] keywords = {Syntax.TRUE.getSyntax(), Syntax.FALSE.getSyntax()};
        String[] binaryOperators = {Syntax.EQUALS_EQUALS.getSyntax(),
                                    Syntax.NOT_EQUALS.getSyntax(),
                                    Syntax.AND.getSyntax(),
                                    Syntax.OR.getSyntax()};

        for (String keyword1 : keywords)
        {
            for (String keyword2 : keywords)
            {
                for (String op : binaryOperators)
                {
                    String expression = keyword1 + op + keyword2;
                    assertEquals(expectedExpression, expressionTypeOf(expression), message + expression);
                }
            }
        }
    }

    @Test
    public void parserIdentifiesParenthesizedExpression()
    {
        TokenType expectedExpression = TokenType.PARENTHESIZED_EXPRESSION_TOKEN;
        String message = "Failed to identify parenthesized expression: ";

        String[] inputSyntax = {"0", "1", "10000", "123456789", "-1", "-10000", "-123456789",
                                "10 + 1", "(10 + 5)", "(10 + 5) * 10", "var = true", "true == false"};

        for (String syntax : inputSyntax)
        {
            String parenthesizedSyntax = Syntax.OPEN_PARENTHESIS.getSyntax() +
                                         syntax +
                                         Syntax.CLOSE_PARENTHESIS.getSyntax();
            assertEquals(expectedExpression, expressionTypeOf(parenthesizedSyntax), message + expectedExpression);
        }
    }

    @Test
    public void parserIdentifiesNameExpression()
    {
        TokenType expectedExpression = TokenType.NAME_EXPRESSION_TOKEN;
        String message = "Failed to identify name expression: ";

        String[] inputSyntax = {"a", "aaa", "var", "myVeryLongVariableName"};

        for (String syntax : inputSyntax)
            assertEquals(expectedExpression, expressionTypeOf(syntax), message + syntax);
    }

    @Test
    public void parserIdentifiesAssignmentExpression()
    {
        TokenType expectedExpression = TokenType.ASSIGNMENT_EXPRESSION_TOKEN;
        String message = "Failed to identify assignment expression: ";

        String[] inputSyntax = {"a = 1", "aaa = 10", "var = a", "myVeryLongVariableName = 50", "A = b"};

        for (String syntax : inputSyntax)
            assertEquals(expectedExpression, expressionTypeOf(syntax), message + syntax);
    }

    @NotNull
    @Contract(pure = true)
    private static String appendUnaryOperator(@NotNull String operator, int value)
    {
        if (operator.equals("("))
            return operator + value + ")";
        return operator + value;
    }

    private static TokenType expressionTypeOf(String input)
    {
        Parser parser = createParser(input);
        Expression expression = parser.getParseTree().getExpression();
        return expression.getTokenType();
    }

    @NotNull
    private static Parser createParser(String input)
    {
        ErrorHandler errorHandler = new ErrorHandler(input);
        Lexer lexer = new Lexer(input, errorHandler);
        return new Parser(lexer, errorHandler);
    }
}