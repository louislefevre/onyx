package analysis.lexical;

import errors.ErrorHandler;
import identifiers.TokenType;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class LexerTest
{
    @Test
    public void lexerReturnsCorrectTokens()
    {
        Lexer lexer = createLexer("5+5");

        List<Token> expectedTokens = Arrays.asList(
                new Token(TokenType.NUMBER_TOKEN, "5", 5, 0),
                new Token(TokenType.PLUS_TOKEN, "+", 1),
                new Token(TokenType.NUMBER_TOKEN, "5", 5, 2),
                new Token(TokenType.EOF_TOKEN, "\0", 3)
        );
        List<Token> actualTokens = lexer.getTokens();

        for (int i = 0; i < expectedTokens.size(); i++)
        {
            Token expectedToken = expectedTokens.get(i);
            Token actualToken = actualTokens.get(i);

            assertEquals(expectedToken.getTokenType(), actualToken.getTokenType(),
                         "Failed to return correct tokens - TokenType mismatch");
            assertEquals(expectedToken.getSyntax(), actualToken.getSyntax(),
                         "Failed to return correct tokens - Syntax mismatch");
            assertEquals(expectedToken.getValue(), actualToken.getValue(),
                         "Failed to return correct tokens - Value mismatch");
            assertEquals(expectedToken.getPosition(), actualToken.getPosition(),
                         "Failed to return correct tokens - Position mismatch");
        }
    }

    @Test
    public void lexerReturnsCorrectAmountOfTokens()
    {
        Lexer lexer = createLexer("(10+5)*10");

        List<Token> expectedTokens = Arrays.asList(
                new Token(TokenType.OPEN_PARENTHESIS_TOKEN, "(", 0),
                new Token(TokenType.NUMBER_TOKEN, "10", 10, 1),
                new Token(TokenType.PLUS_TOKEN, "+", 2),
                new Token(TokenType.NUMBER_TOKEN, "5", 5, 3),
                new Token(TokenType.CLOSE_PARENTHESIS_TOKEN, ")", 4),
                new Token(TokenType.STAR_TOKEN, "*", 5),
                new Token(TokenType.NUMBER_TOKEN, "10", 10, 6),
                new Token(TokenType.EOF_TOKEN, "\0", 7)
        );
        List<Token> actualTokens = lexer.getTokens();

        assertEquals(expectedTokens.size(), actualTokens.size(),
                     "Incorrect token count");
    }

    @Test
    public void lexerRemovesWhitespaceTokens()
    {
        Lexer lexer = createLexer("  5  *     2");
        List<Token> tokens = lexer.getTokens();

        String expectedSyntax = "5*2\0";
        StringBuilder actualSyntax = new StringBuilder();

        for (Token token : tokens)
            actualSyntax.append(token.getSyntax());

        assertEquals(expectedSyntax, actualSyntax.toString(),
                     "Failed to remove whitespace");
    }

    @Test
    public void lexerIdentifiesEOFToken()
    {
        Lexer lexer = createLexer("true == false");
        List<Token> tokens = lexer.getTokens();
        Token token = tokens.get(tokens.size() - 1);

        TokenType expectedTokenType = TokenType.EOF_TOKEN;
        TokenType actualTokenType = token.getTokenType();
        String expectedSyntax = "\0";
        String actualSyntax = token.getSyntax();

        assertEquals(expectedTokenType, actualTokenType,
                     "Failed to identify whitespace - TokenType mismatch");
        assertEquals(expectedSyntax, actualSyntax,
                     "Failed to identify whitespace - Syntax mismatch");
    }

    @Test
    public void lexerIdentifiesNumberToken()
    {
        TokenType type = TokenType.NUMBER_TOKEN;
        String message = "Failed to identify number token";

        identifyToken("0", type, message);
        identifyToken("110", type, message);
        identifyToken("10000000", type, message);
    }

    @Test
    public void lexerIdentifiesIdentifierToken()
    {
        TokenType type = TokenType.IDENTIFIER_KEYWORD_TOKEN;
        String message = "Failed to identify identifier token";

        identifyToken("a", type, message);
        identifyToken("var", type, message);
        identifyToken("myVeryLongIdentifierNameForTesting", type, message);
    }

    @Test
    public void lexerIdentifiesKeywordToken()
    {
        String message = "Failed to identify keyword token";

        identifyToken("true", TokenType.TRUE_KEYWORD_TOKEN, message);
        identifyToken("false", TokenType.FALSE_KEYWORD_TOKEN, message);
    }

    @Test
    public void lexerIdentifiesSymbolToken()
    {
        String message = "Failed to identify symbol token";

        identifyToken(Syntax.PLUS.getSyntax(), TokenType.PLUS_TOKEN, message);
        identifyToken(Syntax.MINUS.getSyntax(), TokenType.MINUS_TOKEN, message);
        identifyToken(Syntax.STAR.getSyntax(), TokenType.STAR_TOKEN, message);
        identifyToken(Syntax.SLASH.getSyntax(), TokenType.SLASH_TOKEN, message);
        identifyToken(Syntax.CARET.getSyntax(), TokenType.CARET_TOKEN, message);
        identifyToken(Syntax.PERCENT.getSyntax(), TokenType.PERCENT_TOKEN, message);
        identifyToken(Syntax.OPEN_PARENTHESIS.getSyntax(), TokenType.OPEN_PARENTHESIS_TOKEN, message);
        identifyToken(Syntax.CLOSE_PARENTHESIS.getSyntax(), TokenType.CLOSE_PARENTHESIS_TOKEN, message);
        identifyToken(Syntax.AND.getSyntax(), TokenType.AND_TOKEN, message);
        identifyToken(Syntax.OR.getSyntax(), TokenType.OR_TOKEN, message);
        identifyToken(Syntax.EQUALS_EQUALS.getSyntax(), TokenType.EQUALS_EQUALS_TOKEN, message);
        identifyToken(Syntax.EQUALS.getSyntax(), TokenType.EQUALS_TOKEN, message);
        identifyToken(Syntax.NOT_EQUALS.getSyntax(), TokenType.NOT_EQUALS_TOKEN, message);
        identifyToken(Syntax.NOT.getSyntax(), TokenType.NOT_TOKEN, message);
        identifyToken(Syntax.GREATER.getSyntax(), TokenType.GREATER_TOKEN, message);
        identifyToken(Syntax.LESS.getSyntax(), TokenType.LESS_TOKEN, message);
        identifyToken(Syntax.GREATER_EQUALS.getSyntax(), TokenType.GREATER_EQUALS_TOKEN, message);
        identifyToken(Syntax.LESS_EQUALS.getSyntax(), TokenType.LESS_EQUALS_TOKEN, message);
        identifyToken(Syntax.EOF.getSyntax(), TokenType.EOF_TOKEN, message);
    }

    @NotNull
    private static Lexer createLexer(String input)
    {
        ErrorHandler errorHandler = new ErrorHandler(input);
        return new Lexer(input, errorHandler);
    }

    private void identifyToken(String input, TokenType expectedTokenType, String message)
    {
        Lexer lexer = createLexer(input);
        Token token = lexer.getTokens().get(0);
        TokenType actualTokenType = token.getTokenType();

        assertEquals(expectedTokenType, actualTokenType, message);
    }
}
