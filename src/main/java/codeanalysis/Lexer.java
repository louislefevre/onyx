package main.java.codeanalysis;

import java.util.ArrayList;
import java.util.List;

public class Lexer
{
    private final String text;
    private int position;
    private List<String> diagnostics;

    public Lexer(String text)
    {
        this.text = text;
        this.diagnostics = new ArrayList<>();
    }

    private char current()
    {
        if(this.position >= this.text.length())
            return '\0';
        return this.text.charAt(this.position);
    }

    public List<String> getDiagnostics() { return this.diagnostics; }

    private void next()
    {
        this.position++;
    }

    public SyntaxToken nextToken()
    {
        if(this.position >= this.text.length())
            return new SyntaxToken(SyntaxKind.EndOfFileToken, this.position, "\0", null);

        if(Character.isDigit(this.current()))
        {
            int start = this.position;

            while(Character.isDigit(this.current()))
                this.next();

            String text = this.text.substring(start, this.position);
            int value = 0;

            if(Utilities.isParsable(text))
                value = Integer.parseInt(text);
            else
                this.diagnostics.add(String.format("The number '%s' isn't a valid Int32", this.text));

            return new SyntaxToken(SyntaxKind.NumberToken, start, text, value);
        }

        if(Character.isWhitespace(this.current()))
        {
            int start = this.position;

            while(Character.isWhitespace(this.current()))
                this.next();

            String text = this.text.substring(start, this.position);

            return new SyntaxToken(SyntaxKind.WhiteSpace, start, text, null);
        }

        switch(this.current())
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
                this.diagnostics.add(String.format("ERROR: Bad character '%s'", this.current()));
                return new SyntaxToken(SyntaxKind.BadToken, this.position++, text.substring(this.position-1, this.position), null);
        }
    }
}
