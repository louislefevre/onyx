package main.java.codeanalysis;

import java.util.ArrayList;
import java.util.List;

public class Lexer
{
    private final String inputText;
    private List<String> diagnosticsLog;
    private int position;

    public Lexer(String inputText)
    {
        this.inputText = inputText;
        this.diagnosticsLog = new ArrayList<>();
        this.position = 0;
    }

    public Token nextToken()
    {
        if(this.position >= this.inputText.length())
            return this.endToken();
        else if(Character.isDigit(this.currentChar()))
            return this.numberToken();
        else if(Character.isWhitespace(this.currentChar()))
            return this.whitespaceToken();
        return this.symbolToken();
    }

    private Token endToken()
    {
        return new Token(TokenType.EndOfFileToken, this.position, "\0", null);
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

        return new Token(TokenType.NumberToken, startPos, text, value);
    }

    private Token whitespaceToken()
    {
        int startPos = this.position;

        while(Character.isWhitespace(this.currentChar()))
            this.nextPosition();

        String text = this.inputText.substring(startPos, this.position);

        return new Token(TokenType.WhiteSpaceToken, startPos, text, null);
    }

    private Token symbolToken()
    {
        switch(this.currentChar())
        {
            case '+':
                return new Token(TokenType.PlusToken, this.position++, "+", null);
            case '-':
                return new Token(TokenType.MinusToken, this.position++, "-", null);
            case '*':
                return new Token(TokenType.StarToken, this.position++, "*", null);
            case '/':
                return new Token(TokenType.SlashToken, this.position++, "/", null);
            case '(':
                return new Token(TokenType.OpenParenthesisToken, this.position++, "(", null);
            case ')':
                return new Token(TokenType.CloseParenthesisToken, this.position++, ")", null);
            default:
                this.diagnosticsLog.add(String.format("ERROR: Bad character '%s'", this.currentChar()));
                return new Token(TokenType.BadToken, this.position++, inputText.substring(this.position-1, this.position), null);
        }
    }

    private char currentChar()
    {
        if(this.position >= this.inputText.length())
            return '\0';
        return this.inputText.charAt(this.position);
    }

    private void nextPosition()
    {
        this.position++;
    }

    public List<String> getDiagnosticsLog()
    {
        return this.diagnosticsLog;
    }
}
