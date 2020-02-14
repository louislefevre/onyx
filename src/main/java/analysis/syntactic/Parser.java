package analysis.syntactic;

import analysis.lexical.Lexer;
import analysis.lexical.Token;
import analysis.lexical.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

public final class Parser
{
    private final Lexer lexer;
    private final List<Token> tokens;
    @Getter private final List<String> diagnosticsLog;
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

    public Expression getExpression()
    {
        return this.parseExpression(0);
    }

    private void addTokens()
    {
        Token token;
        do
        {
            token = this.lexer.nextToken();
            if(token.getType() != TokenType.BadToken && token.getType() != TokenType.WhiteSpaceToken)
                this.tokens.add(token);
        }while(token.getType() != TokenType.EOFToken);
    }

    private void addDiagnostics()
    {
        this.diagnosticsLog.addAll(this.lexer.getDiagnosticsLog());
    }

    private Token matchTokens(TokenType kind)
    {
        if(this.currentToken().getType() == kind)
            return this.nextToken();
        this.diagnosticsLog.add(String.format("ERROR: Unexpected token '%1s', expected '%2s'", this.currentToken().getType(), kind));
        return new Token(kind, this.currentToken().getPosition());
    }

    private Expression parseExpression(int parentPrecedence)
    {
        Expression left;
        int unaryOperatorPrecedence = Syntax.getUnaryOperatorPrecedence(this.currentToken().getType());

        if(unaryOperatorPrecedence != 0 && unaryOperatorPrecedence >= parentPrecedence)
        {
            Token operatorToken = this.nextToken();
            Expression operand = this.parseExpression(unaryOperatorPrecedence);
            left = new UnaryExpression(operatorToken, operand);
        }
        else
        {
            left = this.parsePrimaryExpression();
        }

        while(true)
        {
            int precedence = Syntax.getBinaryOperatorPrecedence(this.currentToken().getType());
            if(precedence == 0 || precedence <= parentPrecedence)
                break;

            Token operatorToken = this.nextToken();
            Expression right = this.parseExpression(precedence);
            left = new BinaryExpression(left, operatorToken, right);
        }

        return left;
    }

    private Expression parsePrimaryExpression()
    {
        if(this.currentToken().getType() == TokenType.OpenParenthesisToken)
        {
            Token left = this.nextToken();
            Expression expression = this.parseExpression(0);
            Token right = this.matchTokens(TokenType.CloseParenthesisToken);
            return new ParenthesizedExpression(left, expression, right);
        }
        else if(this.currentToken().getType() == TokenType.TrueKeywordToken || this.currentToken().getType() == TokenType.FalseKeywordToken)
        {
            boolean value = false;

            if(this.currentToken().getType() == TokenType.TrueKeywordToken)
                value = true;
            else if(this.currentToken().getType() == TokenType.FalseKeywordToken)
                value = false;

            return new LiteralExpression(this.nextToken(), value);
        }

        Token numberToken = this.matchTokens(TokenType.NumberToken);
        return new LiteralExpression(numberToken);
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
}
