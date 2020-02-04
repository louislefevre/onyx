package main.java.codeanalysis;

import java.util.ArrayList;
import java.util.List;

public class Parser
{
    private Lexer lexer;
    private List<Token> tokens;
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
        Token token;
        do
        {
            token = this.lexer.nextToken();
            if(token.getType() != TokenType.BadToken && token.getType() != TokenType.WhiteSpaceToken)
                this.tokens.add(token);
        }while(token.getType() != TokenType.EndOfFileToken);
    }

    private void addDiagnostics()
    {
        this.diagnosticsLog.addAll(this.lexer.getDiagnosticsLog());
    }

    private Token match(TokenType kind)
    {
        if(this.currentToken().getType() == kind)
            return this.nextToken();
        this.diagnosticsLog.add(String.format("ERROR: Unexpected token '%1s', expected '%2s'", this.currentToken().getType(), kind));
        return new Token(kind, this.currentToken().getPosition(), null, null);
    }

    private Expression parseExpression()
    {
        return this.parseTerm();
    }

    private Expression parseTerm()
    {
        Expression leftTerm = this.parseFactor();

        while(this.currentToken().getType() == TokenType.PlusToken || this.currentToken().getType() == TokenType.MinusToken)
        {
            Token operatorToken = this.nextToken();
            Expression rightTerm = this.parseFactor();
            leftTerm = new BinaryExpression(leftTerm, operatorToken, rightTerm);
        }

        return leftTerm;
    }

    private Expression parseFactor()
    {
        Expression leftTerm = this.parsePrimaryExpression();

        while(this.currentToken().getType() == TokenType.StarToken || this.currentToken().getType() == TokenType.SlashToken)
        {
            Token operatorToken = this.nextToken();
            Expression rightTerm = this.parsePrimaryExpression();
            leftTerm = new BinaryExpression(leftTerm, operatorToken, rightTerm);
        }

        return leftTerm;
    }

    private Expression parsePrimaryExpression()
    {
        if(this.currentToken().getType() == TokenType.OpenParenthesisToken)
        {
            Token left = this.nextToken();
            Expression expression = this.parseExpression();
            Token right = this.match(TokenType.CloseParenthesisToken);
            return new ParenthesizedExpression(left, expression, right);
        }

        Token numberToken = this.match(TokenType.NumberToken);
        return new NumberExpression(numberToken);
    }

    private Token nextToken()
    {
        Token token = this.currentToken();
        this.position++;
        return token;
    }

    private Token currentToken()
    {
        return peekToken(0);
    }

    private Token peekToken(int peekPos)
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

    public Expression getExpression()
    {
        return this.parseExpression();
    }
}
