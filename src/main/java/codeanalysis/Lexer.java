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

    public SyntaxToken nextToken()
    {
        if(this.position >= this.inputText.length())
            return this.endToken();
        else if(Character.isDigit(this.currentChar()))
            return this.numberToken();
        else if(Character.isWhitespace(this.currentChar()))
            return this.whitespaceToken();
        return this.symbolToken();
    }

    private SyntaxToken endToken()
    {
        return new SyntaxToken(SyntaxKind.EndOfFileToken, this.position, "\0", null);
    }

    private SyntaxToken numberToken()
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

        return new SyntaxToken(SyntaxKind.NumberToken, startPos, text, value);
    }

    private SyntaxToken whitespaceToken()
    {
        int startPos = this.position;

        while(Character.isWhitespace(this.currentChar()))
            this.nextPosition();

        String text = this.inputText.substring(startPos, this.position);

        return new SyntaxToken(SyntaxKind.WhiteSpace, startPos, text, null);
    }

    private SyntaxToken symbolToken()
    {
        switch(this.currentChar())
        {
            case '+':
                return new SyntaxToken(SyntaxKind.PlusToken, this.position++, "+", null);
            case '-':
                return new SyntaxToken(SyntaxKind.MinusToken, this.position++, "-", null);
            case '*':
                return new SyntaxToken(SyntaxKind.StarToken, this.position++, "*", null);
            case '/':
                return new SyntaxToken(SyntaxKind.SlashToken, this.position++, "/", null);
            case '(':
                return new SyntaxToken(SyntaxKind.OpenParenthesisToken, this.position++, "(", null);
            case ')':
                return new SyntaxToken(SyntaxKind.CloseParenthesisToken, this.position++, ")", null);
            default:
                this.diagnosticsLog.add(String.format("ERROR: Bad character '%s'", this.currentChar()));
                return new SyntaxToken(SyntaxKind.BadToken, this.position++, inputText.substring(this.position-1, this.position), null);
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
