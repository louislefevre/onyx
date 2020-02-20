package analysis.syntactic;

import analysis.lexical.Lexer;
import analysis.lexical.Token;
import analysis.identifiers.TokenType;
import analysis.syntax.Syntax;
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
            if(token.getTokenType() != TokenType.BadToken && token.getTokenType() != TokenType.WhiteSpaceToken)
                this.tokens.add(token);
        }while(token.getTokenType() != TokenType.EOFToken);
    }

    private void addDiagnostics()
    {
        this.diagnosticsLog.addAll(this.lexer.getDiagnosticsLog());
    }

    private Token matchTokens(TokenType kind)
    {
        if(this.currentToken().getTokenType() == kind)
            return this.nextToken();
        this.diagnosticsLog.add(String.format("ERROR: Unexpected token '%1s', expected '%2s'", this.currentToken().getTokenType(), kind));
        return new Token(kind, this.currentToken().getPosition());
    }

    private Expression parseExpression(int parentPrecedence)
    {
        Expression left;
        int unaryOperatorPrecedence = Syntax.getUnaryOperatorPrecedence(this.currentToken().getTokenType());

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
            int precedence = Syntax.getBinaryOperatorPrecedence(this.currentToken().getTokenType());
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
        switch(this.currentToken().getTokenType())
        {
            case OpenParenthesisToken:
                Token left = this.nextToken();
                Expression expression = this.parseExpression(0);
                Token right = this.matchTokens(TokenType.CloseParenthesisToken);
                return new ParenthesizedExpression(left, expression, right);

            case FalseKeyword:
            case TrueKeyword:
                Token keywordToken = this.nextToken();
                boolean value = keywordToken.getTokenType() == TokenType.TrueKeyword;
                return new LiteralExpression(keywordToken, value);

            default:
                Token numberToken = this.matchTokens(TokenType.NumberToken);
                return new LiteralExpression(numberToken);
        }
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
