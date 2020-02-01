package main.java.codeanalysis;

public class Lexer
{
    private final String text;
    private int position;

    public Lexer(String text)
    {
        this.text = text;
    }

    private char current()
    {
        if(this.position >= this.text.length())
            return '\0';
        return this.text.charAt(this.position);
    }

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
            int value = Integer.parseInt(text);

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
                return new SyntaxToken(SyntaxKind.BadToken, this.position++, text.substring(this.position-1, 1), null);
        }
    }
}
