package main.java.codeanalysis;

import java.util.ArrayList;
import java.util.List;

public class Parser
{
    private Lexer lexer;
    private List<SyntaxToken> tokens;
    private List<String> diagnosticsLog;
    private int position;

    public Parser(Lexer lexer)
    {
        this.lexer = lexer;
        this.tokens = new ArrayList<>();
        this.diagnosticsLog = new ArrayList<>();
        this.position = 0;
        this.addTokens();
        this.addDiagnostics();
    }

    private void addTokens()
    {
        SyntaxToken token;
        do
        {
            token = this.lexer.nextToken();
            if(token.getKind() != SyntaxKind.BadToken && token.getKind() != SyntaxKind.WhiteSpace)
                this.tokens.add(token);
        }while(token.getKind() != SyntaxKind.EndOfFileToken);
    }

    private void addDiagnostics()
    {
        this.diagnosticsLog.addAll(this.lexer.getDiagnosticsLog());
    }

    private SyntaxToken match(SyntaxKind kind)
    {
        if(this.currentToken().getKind() == kind)
            return this.nextToken();
        this.diagnosticsLog.add(String.format("ERROR: Unexpected token '%1s', expected '%2s'", this.currentToken().getKind(), kind));
        return new SyntaxToken(kind, this.currentToken().getPosition(), null, null);
    }

    private ExpressionSyntax parseExpression()
    {
        return this.parseTerm();
    }

    private ExpressionSyntax parseTerm()
    {
        ExpressionSyntax left = this.parseFactor();

        while(this.currentToken().getKind() == SyntaxKind.PlusToken || this.currentToken().getKind() == SyntaxKind.MinusToken)
        {
            SyntaxToken operatorToken = this.nextToken();
            ExpressionSyntax right = this.parseFactor();
            left = new BinaryExpressionSyntax(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionSyntax parseFactor()
    {
        ExpressionSyntax left = this.parsePrimaryExpression();

        while(this.currentToken().getKind() == SyntaxKind.StarToken || this.currentToken().getKind() == SyntaxKind.SlashToken)
        {
            SyntaxToken operatorToken = this.nextToken();
            ExpressionSyntax right = this.parsePrimaryExpression();
            left = new BinaryExpressionSyntax(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionSyntax parsePrimaryExpression()
    {
        if(this.currentToken().getKind() == SyntaxKind.OpenParenthesisToken)
        {
            SyntaxToken left = this.nextToken();
            ExpressionSyntax expression = this.parseExpression();
            SyntaxToken right = this.match(SyntaxKind.CloseParenthesisToken);
            return new ParenthesizedExpressionSyntax(left, expression, right);
        }

        SyntaxToken numberToken = this.match(SyntaxKind.NumberToken);
        return new NumberExpressionSyntax(numberToken);
    }

    private SyntaxToken nextToken()
    {
        SyntaxToken current = this.currentToken();
        this.position++;
        return current;
    }

    private SyntaxToken currentToken()
    {
        return peekToken(0);
    }

    private SyntaxToken peekToken(int peekPos)
    {
        int index = this.position + peekPos;
        if(index >= this.tokens.size())
            return this.tokens.get(this.tokens.size() - 1);
        return this.tokens.get(index);
    }

    public List<String> getDiagnosticsLog()
    {
        return this.diagnosticsLog;
    }

    public ExpressionSyntax getExpression()
    {
        return this.parseExpression();
    }
}
