package analysis.syntax;

import errors.ErrorHandler;
import symbols.Syntax;
import symbols.TokenType;
import analysis.lexical.Lexer;
import analysis.lexical.Token;

import java.util.List;

public final class Parser
{
    private final List<Token> tokens;
    private int position;

    public Parser(Lexer lexer)
    {
        this.tokens = lexer.getTokens();
        this.position = 0;
    }

    public Expression getExpression()
    {
        return this.parseExpression(0);
    }

    private Token matchTokens(TokenType kind)
    {
        if(this.currentToken().getTokenType() == kind)
            return this.nextToken();
        ErrorHandler.addSyntaxError(String.format("ERROR: Unexpected token '%1s', expected '%2s'", this.currentToken().getTokenType(), kind));
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
        int index = this.position;
        if(index >= this.tokens.size())
            return this.tokens.get(this.tokens.size() - 1);
        return this.tokens.get(index);
    }
}
