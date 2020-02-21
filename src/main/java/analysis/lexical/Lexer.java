package analysis.lexical;

import errors.ErrorHandler;
import util.Utilities;
import identifiers.TokenType;

import java.util.ArrayList;
import java.util.List;

public final class Lexer
{
    private final String inputText;

    private int position;

    public Lexer(String inputText)
    {
        this.inputText = inputText;
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
            if(token.getTokenType() != TokenType.BAD_TOKEN && token.getTokenType() != TokenType.WHITE_SPACE_TOKEN)
                tokens.add(token);
        }while(token.getTokenType() != TokenType.EOF_TOKEN);
        return tokens;
    }

    private Token nextToken()
    {
        if(this.position >= this.inputText.length())
            return this.endToken();
        else if(Character.isDigit(this.currentChar()))
            return this.numberToken();
        else if(Character.isLetter(this.currentChar()))
            return this.letterToken();
        else if(Character.isWhitespace(this.currentChar()))
            return this.whitespaceToken();
        return this.symbolToken();
    }

    private Token endToken()
    {
        return new Token(TokenType.EOF_TOKEN, "\0", this.position);
    }

    private Token numberToken()
    {
        int startPos = this.position;
        int value = 0;

        while(Character.isDigit(this.currentChar()))
            this.nextPosition();

        String text = this.inputText.substring(startPos, this.position);

        if(Utilities.isParsable(text))
            value = Integer.parseInt(text);
        else
            ErrorHandler.addLexicalError(String.format("The number '%s' isn't a valid Int32", this.inputText));

        return new Token(TokenType.NUMBER_TOKEN, text, value, startPos);
    }

    private Token letterToken()
    {
        int startPos = this.position;

        while(Character.isLetter(this.currentChar()))
            this.nextPosition();

        String text = this.inputText.substring(startPos, this.position);
        TokenType kind = getKeywordKind(text);

        return new Token(kind, text, startPos);
    }

    private Token whitespaceToken()
    {
        int startPos = this.position;

        while(Character.isWhitespace(this.currentChar()))
            this.nextPosition();

        String text = this.inputText.substring(startPos, this.position);

        return new Token(TokenType.WHITE_SPACE_TOKEN, text, startPos);
    }

    private Token symbolToken()
    {
        switch(this.currentChar())
        {
            case '+':
                return new Token(TokenType.PLUS_TOKEN, "+", this.position++);
            case '-':
                return new Token(TokenType.MINUS_TOKEN, "-", this.position++);
            case '*':
                return new Token(TokenType.STAR_TOKEN, "*", this.position++);
            case '/':
                return new Token(TokenType.SLASH_TOKEN, "/", this.position++);
            case '(':
                return new Token(TokenType.OPEN_PARENTHESIS_TOKEN, "(", this.position++);
            case ')':
                return new Token(TokenType.CLOSE_PARENTHESIS_TOKEN, ")", this.position++);
            case '&':
                if(this.nextChar() == '&')
                    return new Token(TokenType.AND_TOKEN, "&&", this.position+=2);
            case '|':
                if(this.nextChar() == '|')
                    return new Token(TokenType.OR_TOKEN, "||", this.position += 2);
            case '=':
                if(this.nextChar() == '=')
                    return new Token(TokenType.EQUALS_TOKEN, "==", this.position += 2);
            case '!':
                if(this.nextChar() == '=')
                    return new Token(TokenType.NOT_EQUALS_TOKEN, "!=", this.position += 2);
                return new Token(TokenType.BANG_TOKEN, "!", this.position++);
            default:
                ErrorHandler.addLexicalError(String.format("ERROR: Bad character '%s'", this.currentChar()));
                return new Token(TokenType.BAD_TOKEN, inputText.substring(Utilities.minimumZero(this.position-1), this.position), this.position++);
        }
    }

    private char currentChar()
    {
        return this.peek(0);
    }

    private char nextChar()
    {
        return this.peek(1);
    }

    private char peek(int offset)
    {
        int index = this.position + offset;

        if(index >= this.inputText.length())
            return '\0';
        return this.inputText.charAt(index);
    }

    private void nextPosition()
    {
        this.position++;
    }

    private static TokenType getKeywordKind(String text)
    {
        switch(text)
        {
            case "true":
                return TokenType.TRUE_KEYWORD_TOKEN;
            case "false":
                return TokenType.FALSE_KEYWORD_TOKEN;
            default:
                return TokenType.IDENTIFIER_KEYWORD_TOKEN;
        }
    }
}
