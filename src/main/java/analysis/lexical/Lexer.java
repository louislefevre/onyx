package analysis.lexical;

import symbols.TokenType;
import symbols.Syntax;
import misc.Utilities;

import java.util.ArrayList;
import java.util.List;

public final class Lexer
{
    private final String inputText;
    private final List<String> diagnosticsLog;
    private int position;

    public Lexer(String inputText)
    {
        this.inputText = inputText;
        this.diagnosticsLog = new ArrayList<>();
        this.position = 0;
    }

    public List<Token> getTokens()
    {
        return this.lexTokens();
    }

    public List<String> getDiagnosticsLog()
    {
        return this.diagnosticsLog;
    }

    private List<Token> lexTokens()
    {
        List<Token> tokens = new ArrayList<>();
        Token token;
        do
        {
            token = this.nextToken();
            if(token.getTokenType() != TokenType.BadToken && token.getTokenType() != TokenType.WhiteSpaceToken)
                tokens.add(token);
        }while(token.getTokenType() != TokenType.EOFToken);
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
        return new Token(TokenType.EOFToken, "\0", this.position);
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
            this.diagnosticsLog.add(String.format("The number '%s' isn't a valid Int32", this.inputText));

        return new Token(TokenType.NumberToken, text, value, startPos);
    }

    private Token letterToken()
    {
        int startPos = this.position;

        while(Character.isLetter(this.currentChar()))
            this.nextPosition();

        String text = this.inputText.substring(startPos, this.position);
        TokenType kind = Syntax.getKeywordKind(text);

        return new Token(kind, text, startPos);
    }

    private Token whitespaceToken()
    {
        int startPos = this.position;

        while(Character.isWhitespace(this.currentChar()))
            this.nextPosition();

        String text = this.inputText.substring(startPos, this.position);

        return new Token(TokenType.WhiteSpaceToken, text, startPos);
    }

    private Token symbolToken()
    {
        switch(this.currentChar())
        {
            case '+':
                return new Token(TokenType.PlusToken, "+", this.position++);
            case '-':
                return new Token(TokenType.MinusToken, "-", this.position++);
            case '*':
                return new Token(TokenType.StarToken, "*", this.position++);
            case '/':
                return new Token(TokenType.SlashToken, "/", this.position++);
            case '(':
                return new Token(TokenType.OpenParenthesisToken, "(", this.position++);
            case ')':
                return new Token(TokenType.CloseParenthesisToken, ")", this.position++);
            case '&':
                if(this.nextChar() == '&')
                    return new Token(TokenType.AndToken, "&&", this.position+=2);
            case '|':
                if(this.nextChar() == '|')
                    return new Token(TokenType.OrToken, "||", this.position += 2);
            case '=':
                if(this.nextChar() == '=')
                    return new Token(TokenType.EqualsToken, "==", this.position += 2);
            case '!':
                if(this.nextChar() == '=')
                    return new Token(TokenType.NotEqualsToken, "!=", this.position += 2);
                return new Token(TokenType.BangToken, "!", this.position++);
            default:
                this.diagnosticsLog.add(String.format("ERROR: Bad character '%s'", this.currentChar()));
                return new Token(TokenType.BadToken, inputText.substring(Utilities.minimumZero(this.position-1), this.position), this.position++);
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
}
