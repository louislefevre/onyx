package analysis.lexical;

import errors.ErrorHandler;
import errors.LexicalError;
import identifiers.TokenType;
import source.SourceInput;
import util.Utilities;

import java.util.ArrayList;
import java.util.List;

public final class Lexer
{
    private final SourceInput sourceInput;
    private final ErrorHandler errorHandler;
    private int position;

    public Lexer(SourceInput sourceInput, ErrorHandler errorHandler)
    {
        this.sourceInput = sourceInput;
        this.errorHandler = errorHandler;
        this.position = 0;
    }

    public List<Token> getTokens()
    {
        return this.lexTokens();
    }

    private List<Token> lexTokens()
    {
        List<Token> tokens = new ArrayList<>();
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
        if (this.position >= this.sourceInput.length())
            return this.endToken();
        else if (Utilities.isWhitespace(this.currentChar()))
            return this.whitespaceToken();
        else if (Utilities.isDigit(this.currentChar()))
            return this.numberToken();
        else if (Utilities.isLetter(this.currentChar()))
            return this.letterToken();
        return this.symbolToken();
    }

    private Token endToken()
    {
        return new Token(TokenType.EOF_TOKEN, this.position);
    }

    private Token whitespaceToken()
    {
        int startPos = this.position;

        while (Utilities.isWhitespace(this.currentChar()))
            this.nextPosition();

        String text = this.sourceInput.substring(startPos, this.position);

        return new Token(TokenType.WHITE_SPACE_TOKEN, text, startPos);
    }

    private Token numberToken()
    {
        int startPos = this.position;
        int value = 0;

        while (Utilities.isDigit(this.currentChar()))
            this.currentPositionThenNext(1);

        String text = this.sourceInput.substring(startPos, this.position);

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

    private Token letterToken()
    {
        int startPos = this.position;

        while (Utilities.isLetter(this.currentChar()))
            this.nextPosition();

        String text = this.sourceInput.substring(startPos, this.position);

        return getKeywordToken(text, startPos);
    }

    private static Token getKeywordToken(String text, int pos)
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

    private Token stringToken()
    {
        StringBuilder syntaxBuilder = new StringBuilder(); // Includes quotes
        StringBuilder valueBuilder = new StringBuilder(); // Doesn't include quotes
        TokenType tokenType;

        syntaxBuilder.append(currentChar());
        int startPos = this.currentPositionThenNext(1);

        while (true)
        {
            String currentChar = this.currentChar();

            if (currentChar.equals("\0") || currentChar.equals("\r") || currentChar.equals("\n"))
            {
                tokenType = TokenType.BAD_TOKEN;
                LexicalError error = LexicalError.incompleteString(valueBuilder.toString(), startPos,
                                                                   this.position - startPos);
                this.errorHandler.addError(error);
                break;
            }
            else if (currentChar.equals("\""))
            {
                tokenType = TokenType.STRING_TOKEN;
                syntaxBuilder.append(currentChar);
                this.nextPosition();
                break;
            }
            syntaxBuilder.append(currentChar);
            valueBuilder.append(currentChar);
            this.nextPosition();
        }

        return new Token(tokenType, syntaxBuilder.toString(), valueBuilder.toString(), startPos);
    }

    private Token symbolToken()
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
        else if (Syntax.DOUBLE_QUOTES.getSyntax().equals(currentChar))
        {
            return this.stringToken();
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
            return new Token(TokenType.NOT_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.GREATER.getSyntax().equals(currentChar))
        {
            if (Syntax.EQUALS.getSyntax().equals(nextChar))
                return new Token(TokenType.GREATER_EQUALS_TOKEN, this.currentPositionThenNext(2));
            return new Token(TokenType.GREATER_TOKEN, this.currentPositionThenNext(1));
        }
        else if (Syntax.LESS.getSyntax().equals(currentChar))
        {
            if (Syntax.EQUALS.getSyntax().equals(nextChar))
                return new Token(TokenType.LESS_EQUALS_TOKEN, this.currentPositionThenNext(2));
            return new Token(TokenType.LESS_TOKEN, this.currentPositionThenNext(1));
        }

        return this.badToken();
    }

    private Token badToken()
    {
        LexicalError error = LexicalError.badCharacter(this.currentChar(), this.position, 1);
        this.errorHandler.addError(error);
        return new Token(TokenType.BAD_TOKEN,
                         sourceInput.substring(Utilities.minimumZero(this.position - 1), this.position),
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

        if (index >= this.sourceInput.length() || index < 0)
            return Syntax.EOF.getSyntax();
        return Character.toString(this.sourceInput.charAt(index));
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
