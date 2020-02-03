package main.java.codeanalysis;

import java.util.ArrayList;
import java.util.List;

public class Parser
{
    private Lexer lexer;
    private SyntaxToken token;
    private List<SyntaxToken> tokens;
    private List<String> diagnostics;
    private int position;

    public Parser(Lexer lexer)
    {
        this.lexer = lexer;
        this.tokens = new ArrayList<>();
        this.diagnostics = new ArrayList<>();

        do
        {
            this.token = this.lexer.nextToken();

            if(this.token.getKind() != SyntaxKind.WhiteSpace && this.token.getKind() != SyntaxKind.BadToken)
                this.tokens.add(this.token);

        } while(this.token.getKind() != SyntaxKind.EndOfFileToken);

        this.diagnostics.addAll(this.lexer.getDiagnostics());
    }

    public List<String> getDiagnostics() { return this.diagnostics; }

    private SyntaxToken peek(int offset)
    {
        int index = this.position + offset;
        if(index >= this.tokens.size())
            return this.tokens.get(this.tokens.size() - 1);
        return this.tokens.get(index);
    }

    private SyntaxToken current() { return peek(0); }

    private SyntaxToken nextToken()
    {
        SyntaxToken current = this.current();
        this.position++;
        return current;
    }

    private SyntaxToken match(SyntaxKind kind)
    {
        if(this.current().getKind() == kind)
            return this.nextToken();
        this.diagnostics.add(String.format("ERROR: Unexpected token '%1s', expected '%2s'", this.current().getKind(), kind));
        return new SyntaxToken(kind, this.current().getPosition(), null, null);
    }

    private ExpressionSyntax parseExpression()
    {
        return parseTerm();
    }

    public ExpressionSyntax getExpression()
    {
        return this.parseTerm();
    }

    public SyntaxToken getEndOfFileToken()
    {
        return this.match(SyntaxKind.EndOfFileToken);
    }

    public ExpressionSyntax parseTerm()
    {
        ExpressionSyntax left = this.parseFactor();

        while(this.current().getKind() == SyntaxKind.PlusToken || this.current().getKind() == SyntaxKind.MinusToken)
        {
            SyntaxToken operatorToken = this.nextToken();
            ExpressionSyntax right = this.parseFactor();
            left = new BinaryExpressionSyntax(left, operatorToken, right);
        }

        return left;
    }

    public ExpressionSyntax parseFactor()
    {
        ExpressionSyntax left = this.parsePrimaryExpression();

        while(this.current().getKind() == SyntaxKind.StarToken || this.current().getKind() == SyntaxKind.SlashToken)
        {
            SyntaxToken operatorToken = this.nextToken();
            ExpressionSyntax right = this.parsePrimaryExpression();
            left = new BinaryExpressionSyntax(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionSyntax parsePrimaryExpression()
    {
        if(this.current().getKind() == SyntaxKind.OpenParenthesisToken)
        {
            SyntaxToken left = this.nextToken();
            ExpressionSyntax expression = this.parseExpression();
            SyntaxToken right = this.match(SyntaxKind.CloseParenthesisToken);
            return new ParenthesizedExpressionSyntax(left, expression, right);
        }

        SyntaxToken numberToken = this.match(SyntaxKind.NumberToken);
        return new NumberExpressionSyntax(numberToken);
    }
}
