package analysis.syntax;

import analysis.lexical.Lexer;
import analysis.lexical.Token;
import errors.Error;
import errors.SyntaxError;
import identifiers.TokenType;

import java.util.ArrayList;
import java.util.List;

public final class Parser
{
    private final List<Token> tokens;
    private final List<Error> errorLog;
    private int position;

    public Parser(Lexer lexer)
    {
        this.tokens = lexer.getTokens();
        this.errorLog = new ArrayList<>();
        this.position = 0;
    }

    public ParseTree getParseTree()
    {
        return new ParseTree(this.parseExpression(0));
    }

    public List<Error> getErrorLog()
    {
        return this.errorLog;
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
            int binaryOperatorPrecedence = SyntaxPrecedence.getBinaryOperatorPrecedence(this.currentToken().getTokenType());
            if(binaryOperatorPrecedence == 0 || binaryOperatorPrecedence <= parentPrecedence)
                break;

            Token operatorToken = this.nextToken();
            Expression right = this.parseExpression(binaryOperatorPrecedence);
            left = new BinaryExpression(left, operatorToken, right);
        }

        return left;
    }

    private Expression parsePrimaryExpression()
    {
        switch(this.currentToken().getTokenType())
        {
            case OPEN_PARENTHESIS_TOKEN:
                return this.parseParenthesizedExpression();
            case FALSE_KEYWORD_TOKEN:
            case TRUE_KEYWORD_TOKEN:
                return this.parseBooleanExpression();
            default:
                return this.parseNumberExpression();
        }
    }

    private Expression parseParenthesizedExpression()
    {
        Token left = this.nextToken();
        Expression expression = this.parseExpression(0);
        Token right = this.matchTokens(TokenType.CLOSE_PARENTHESIS_TOKEN);
        return new ParenthesizedExpression(left, expression, right);
    }

    private Expression parseBooleanExpression()
    {
        Token keywordToken = this.nextToken();
        boolean value = keywordToken.getTokenType() == TokenType.TRUE_KEYWORD_TOKEN;
        return new LiteralExpression(keywordToken, value);
    }

    private Expression parseNumberExpression()
    {
        Token numberToken = this.matchTokens(TokenType.NUMBER_TOKEN);
        return new LiteralExpression(numberToken);
    }

    private Token matchTokens(TokenType kind)
    {
        if(this.currentToken().getTokenType() == kind)
            return this.nextToken();
        this.errorLog.add(new SyntaxError(String.format("ERROR: Unexpected token '%1s', expected '%2s'", this.currentToken().getTokenType(), kind)));
        return new Token(kind, this.currentToken().getPosition());
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
