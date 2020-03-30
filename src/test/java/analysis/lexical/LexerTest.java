package analysis.lexical;

import errors.ErrorHandler;
import identifiers.TokenType;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class LexerTest
{
    @Test
    public void lexerReturnsCorrectTokens()
    {
        String input = "5+5";
        ErrorHandler errorHandler = new ErrorHandler(input);
        Lexer lexer = new Lexer(input, errorHandler);

        List<Token> expectedTokens = Arrays.asList(
                new Token(TokenType.NUMBER_TOKEN, "5", 5, 0),
                new Token(TokenType.PLUS_TOKEN, "+", 1),
                new Token(TokenType.NUMBER_TOKEN, "5", 5, 2),
                new Token(TokenType.EOF_TOKEN, "\0", 3)
        );
        List<Token> actualTokens = lexer.getTokens();

        for(int i = 0; i < expectedTokens.size(); i++)
        {
            Token expectedToken = expectedTokens.get(i);
            Token actualToken = actualTokens.get(i);
            assertEquals(expectedToken.getTokenType(), actualToken.getTokenType());
            assertEquals(expectedToken.getSyntax(), actualToken.getSyntax());
            assertEquals(expectedToken.getValue(), actualToken.getValue());
            assertEquals(expectedToken.getPosition(), actualToken.getPosition());
        }
    }

    @Test
    public void lexerReturnsCorrectAmountOfTokens()
    {
        String input = "1+1";
        ErrorHandler errorHandler = new ErrorHandler(input);
        Lexer lexer = new Lexer(input, errorHandler);

        List<Token> expectedTokens = Arrays.asList(
                new Token(null, "",0),
                new Token(null, "",0),
                new Token(null, "",0),
                new Token(null, "",0)
        );
        List<Token> actualTokens = lexer.getTokens();

        assertEquals(expectedTokens.size(), actualTokens.size());
    }
}