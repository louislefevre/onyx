package analysis.lexical;

import analysis.SourceText;
import errors.ErrorHandler;
import errors.LexicalError;
import identifiers.TokenType;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import util.Utilities;

import java.util.ArrayList;
import java.util.List;

public final class Lexer
{
    private final SourceText sourceText;
    private final ErrorHandler errorHandler;
    private final List<Token> tokens;
    private int position;

    public Lexer(SourceText sourceText, ErrorHandler errorHandler)
    {
        this.sourceText = sourceText;
        this.errorHandler = errorHandler;
        this.tokens = new ArrayList<>();
        this.position = 0;
    }

    public List<Token> getTokens()
    {
        return this.lexTokens();
    }

    private List<Token> lexTokens()
    {
        Token token;
        do
        {
            token = this.nextToken();
            if (token.getTokenType() != TokenType.BAD_TOKEN &&
                token.getTokenType() != TokenType.WHITE_SPACE_TOKEN)
                tokens.add(token);
        } while (token.getTokenType() != TokenType.EOF_TOKEN);
        return tokens;
    }

    private Token nextToken()
    {
        if (this.position >= this.sourceText.length())
            return this.endToken();
        else if (Utilities.isWhitespace(this.currentChar()))
            return this.whitespaceToken();
        else if (Utilities.isDigit(this.currentChar()))
            return this.numberToken();
        else if (Utilities.isLetter(this.currentChar()))
            return this.letterToken();
        return this.symbolToken();
    }

    @Contract(" -> new")
    private @NotNull Token endToken()
    {
        return new Token(TokenType.EOF_TOKEN, this.position);
    }

    @Contract(" -> new")
    private @NotNull Token whitespaceToken()
    {
        int startPos = this.position;

        while (Utilities.isWhitespace(this.currentChar()))
            this.nextPosition();

        String text = this.sourceText.substring(startPos, this.position);

        return new Token(TokenType.WHITE_SPACE_TOKEN, text, startPos);
    }

    private @NotNull Token numberToken()
    {
        int startPos = this.position;
        int value = 0;

        while (Utilities.isDigit(this.currentChar()))
            this.currentPositionThenNext(1);

        String text = this.sourceText.substring(startPos, this.position);

        if (Utilities.isParsable(text))
        {
            value = Integer.parseInt(text);
        }
        else
        {
            LexicalError error = LexicalError.invalidInt(text, startPos, this.position - startPos);
            this.errorHandler.addError(error);
        }

        return new Token(TokenType.NUMBER_TOKEN, text, value, startPos);
    }

    private @NotNull Token letterToken()
    {
        int startPos = this.position;

        while (Utilities.isLetter(this.currentChar()))
            this.nextPosition();

        String text = this.sourceText.substring(startPos, this.position);

        return getKeywordToken(text, startPos);
    }

    @Contract("_, _ -> new")
    private static @NotNull Token getKeywordToken(String text, int pos)
    {
        if (Syntax.TRUE.getSyntax().equals(text))
            return new Token(TokenType.TRUE_KEYWORD_TOKEN, pos);
        else if (Syntax.FALSE.getSyntax().equals(text))
            return new Token(TokenType.FALSE_KEYWORD_TOKEN, pos);
        else if (Syntax.AND.getSyntax().equals(text))
            return new Token(TokenType.AND_TOKEN, pos);
        else if (Syntax.OR.getSyntax().equals(text))
            return new Token(TokenType.OR_TOKEN, pos);

        return new Token(TokenType.IDENTIFIER_KEYWORD_TOKEN, text, pos);
    }

    private @NotNull Token symbolToken()
    {
        String currentChar = this.currentChar();
        String nextChar = this.nextChar();

        if (Syntax.PLUS.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.PLUS_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.MINUS.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.MINUS_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.STAR.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.STAR_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.SLASH.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.SLASH_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.CARET.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.CARET_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.PERCENT.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.PERCENT_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.OPEN_PARENTHESIS.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.OPEN_PARENTHESIS_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.CLOSE_PARENTHESIS.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.CLOSE_PARENTHESIS_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.EQUALS.getSyntax().equals(currentChar))
        {
            if (Syntax.EQUALS.getSyntax().equals(nextChar))
                return new Token(TokenType.EQUALS_EQUALS_TOKEN, this.currentPositionThenNext(2));
            return new Token(TokenType.EQUALS_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.NOT.getSyntax().equals(currentChar))
        {
            if (Syntax.EQUALS.getSyntax().equals(nextChar))
                return new Token(TokenType.NOT_EQUALS_TOKEN, this.currentPositionThenNext(2));
            return new Token(TokenType.NOT_TOKEN, this.position++);
        }
        else if (Syntax.GREATER.getSyntax().equals(currentChar))
        {
            if (Syntax.EQUALS.getSyntax().equals(nextChar))
                return new Token(TokenType.GREATER_EQUALS_TOKEN, this.currentPositionThenNext(2));
            return new Token(TokenType.GREATER_TOKEN, this.position++);
        }
        else if (Syntax.LESS.getSyntax().equals(currentChar))
        {
            if (Syntax.EQUALS.getSyntax().equals(nextChar))
                return new Token(TokenType.LESS_EQUALS_TOKEN, this.currentPositionThenNext(2));
            return new Token(TokenType.LESS_TOKEN, this.position++);
        }

        return this.badToken();
    }

    @Contract(" -> new")
    private @NotNull Token badToken()
    {
        LexicalError error = LexicalError.badCharacter(this.currentChar(), this.position, 1);
        this.errorHandler.addError(error);
        return new Token(TokenType.BAD_TOKEN,
                         sourceText.substring(Utilities.minimumZero(this.position - 1), this.position),
                         this.currentPositionThenNext(1));
    }

    private String currentChar()
    {
        return this.peek(0);
    }

    private String nextChar()
    {
        return this.peek(1);
    }

    private String peek(int offset)
    {
        int index = this.position + offset;

        if (index >= this.sourceText.length() || index < 0)
            return Syntax.EOF.getSyntax();
        return Character.toString(this.sourceText.charAt(index));
    }

    private void nextPosition()
    {
        this.position++;
    }

    private int currentPositionThenNext(int increment)
    {
        int currentPos = this.position;
        this.position += increment;
        return currentPos;
    }
}
