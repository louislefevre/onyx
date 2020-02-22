package analysis.syntax;

import errors.ErrorHandler;
import identifiers.TokenType;
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

    public ParseTree getParseTree()
    {
        return new ParseTree(this.parseExpression(0));
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
        int unaryOperatorPrecedence = SyntaxPrecedence.getUnaryOperatorPrecedence(this.currentToken().getTokenType());

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
            int precedence = SyntaxPrecedence.getBinaryOperatorPrecedence(this.currentToken().getTokenType());
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
            case OPEN_PARENTHESIS_TOKEN:
                Token left = this.nextToken();
                Expression expression = this.parseExpression(0);
                Token right = this.matchTokens(TokenType.CLOSE_PARENTHESIS_TOKEN);
                return new ParenthesizedExpression(left, expression, right);

            case FALSE_KEYWORD_TOKEN:
            case TRUE_KEYWORD_TOKEN:
                Token keywordToken = this.nextToken();
                boolean value = keywordToken.getTokenType() == TokenType.TRUE_KEYWORD_TOKEN;
                return new LiteralExpression(keywordToken, value);

            default:
                Token numberToken = this.matchTokens(TokenType.NUMBER_TOKEN);
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
