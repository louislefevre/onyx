package main.java.analysis;

import main.java.misc.Utilities;

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
        return new Token(TokenType.EndOfFileToken, "\0", null, this.position);
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

    private Token whitespaceToken()
    {
        int startPos = this.position;

        while(Character.isWhitespace(this.currentChar()))
            this.nextPosition();

        String text = this.inputText.substring(startPos, this.position);

        return new Token(TokenType.WhiteSpaceToken, text, null, startPos);
    }

    private Token symbolToken()
    {
        switch(this.currentChar())
        {
            case '+':
                return new Token(TokenType.PlusToken, "+", null, this.position++);
            case '-':
                return new Token(TokenType.MinusToken, "-", null, this.position++);
            case '*':
                return new Token(TokenType.StarToken, "*", null, this.position++);
            case '/':
                return new Token(TokenType.SlashToken, "/", null, this.position++);
            case '(':
                return new Token(TokenType.OpenParenthesisToken, "(", null, this.position++);
            case ')':
                return new Token(TokenType.CloseParenthesisToken, ")", null, this.position++);
            default:
                this.diagnosticsLog.add(String.format("ERROR: Bad character '%s'", this.currentChar()));
                return new Token(TokenType.BadToken, inputText.substring(this.position-1, this.position), null, this.position++);
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
