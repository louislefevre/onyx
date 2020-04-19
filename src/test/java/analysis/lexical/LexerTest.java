package analysis.lexical;

import errors.ErrorHandler;
import identifiers.TokenType;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class LexerTest
{
    @Test
    public void lexerIdentifiesInbuiltTokens()
    {
        String message = "Failed to return correct inbuilt token - TokenType mismatch at: ";

        String[] inputSyntax = {Syntax.PLUS.getSyntax(), Syntax.MINUS.getSyntax(), Syntax.STAR.getSyntax(),
                                Syntax.SLASH.getSyntax(), Syntax.CARET.getSyntax(), Syntax.PERCENT.getSyntax(),
                                Syntax.OPEN_PARENTHESIS.getSyntax(), Syntax.CLOSE_PARENTHESIS.getSyntax(),
                                Syntax.AND.getSyntax(), Syntax.OR.getSyntax(), Syntax.EQUALS.getSyntax(),
                                Syntax.EQUALS_EQUALS.getSyntax(), Syntax.NOT.getSyntax(), Syntax.NOT_EQUALS.getSyntax(),
                                Syntax.GREATER.getSyntax(), Syntax.LESS.getSyntax(), Syntax.GREATER_EQUALS.getSyntax(),
                                Syntax.LESS_EQUALS.getSyntax(), Syntax.TRUE.getSyntax(),
                                Syntax.FALSE.getSyntax(), Syntax.EOF.getSyntax()};
        TokenType[] expectedTypes = {TokenType.PLUS_TOKEN, TokenType.MINUS_TOKEN, TokenType.STAR_TOKEN,
                                     TokenType.SLASH_TOKEN, TokenType.CARET_TOKEN, TokenType.PERCENT_TOKEN,
                                     TokenType.OPEN_PARENTHESIS_TOKEN, TokenType.CLOSE_PARENTHESIS_TOKEN,
                                     TokenType.AND_TOKEN, TokenType.OR_TOKEN, TokenType.EQUALS_TOKEN,
                                     TokenType.EQUALS_EQUALS_TOKEN, TokenType.NOT_TOKEN, TokenType.NOT_EQUALS_TOKEN,
                                     TokenType.GREATER_TOKEN, TokenType.LESS_TOKEN, TokenType.GREATER_EQUALS_TOKEN,
                                     TokenType.LESS_EQUALS_TOKEN, TokenType.TRUE_KEYWORD_TOKEN,
                                     TokenType.FALSE_KEYWORD_TOKEN, TokenType.EOF_TOKEN};

        for (int i = 0; i < inputSyntax.length; i++)
        {
            TokenType expectedTokenType = expectedTypes[i];
            TokenType actualTokenType = tokenTypeOf(inputSyntax[i]);

            assertEquals(expectedTokenType, actualTokenType, message + inputSyntax[i]);
        }


    }

    @Test
    public void lexerIdentifiesNumberToken()
    {
        TokenType expectedTokenType = TokenType.NUMBER_TOKEN;
        String message = "Failed to return correct number token - TokenType mismatch at: ";

        String[] inputSyntax = {"0", "1", "10000", "123456", "2147483647"};

        for (String syntax : inputSyntax)
        {
            TokenType actualTokenType = tokenTypeOf(syntax);
            assertEquals(expectedTokenType, actualTokenType, message + syntax);
        }
    }

    @Test
    public void lexerIdentifiesIdentifierToken()
    {
        TokenType expectedTokenType = TokenType.IDENTIFIER_KEYWORD_TOKEN;
        String message = "Failed to return correct identifier token - TokenType mismatch at: ";

        String[] inputSyntax = {"a", "aaa", "var", "myVeryLongVariableName"};

        for (String syntax : inputSyntax)
        {
            TokenType actualTokenType = tokenTypeOf(syntax);
            assertEquals(expectedTokenType, actualTokenType, message + syntax);
        }
    }

    @Test
    public void lexerReturnsCorrectAmountOfTokens()
    {
        String message = "Failed to return correct token amount - Incorrect amount at: ";

        String[] inputSyntax = {"0", "1", "10000", "123456789", "-1", "-10000", "-123456789",
                                "10 + 1", "(10 + 5)", "(10 + 5) * 10", "var = true", "true == false"};
        int[] expectedAmounts = {2, 2, 2, 2, 3, 3, 3, 4, 6, 8, 4, 4};

        for (int i = 0; i < inputSyntax.length; i++)
        {
            String syntax = inputSyntax[i];
            Lexer lexer = createLexer(syntax);
            int expectedAmount = expectedAmounts[i];
            int actualAmount = lexer.getTokens().size();

            assertEquals(expectedAmount, actualAmount, message + syntax);
        }
    }

    private static TokenType tokenTypeOf(String input)
    {
        Lexer lexer = createLexer(input);
        Token token = lexer.getTokens().get(0);
        return token.getTokenType();
    }

    @NotNull
    private static Lexer createLexer(String input)
    {
        ErrorHandler errorHandler = new ErrorHandler(input);
        return new Lexer(input, errorHandler);
    }
}
