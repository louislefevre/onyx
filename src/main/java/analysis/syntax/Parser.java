package analysis.syntax;

import analysis.lexical.Lexer;
import analysis.lexical.Token;
import errors.Error;
import errors.SyntaxError;
import identifiers.TokenType;

import java.util.List;

public final class Parser
{
    private final List<Token> tokens;
    private final List<Error> errorLog;
    private int position;

    public Parser(Lexer lexer)
    {
        this.tokens = lexer.getTokens();
        this.errorLog = lexer.getErrorLog();
        this.position = 0;
    }

    public ParseTree getParseTree()
    {
        return new ParseTree(this.parseExpression());
    }

    public List<Error> getErrorLog()
    {
        return this.errorLog;
    }

    private Expression parseExpression()
    {
        return this.parseAssignmentExpression();
    }

    private Expression parseAssignmentExpression()
    {
        if(this.peek(0).getTokenType() == TokenType.IDENTIFIER_KEYWORD_TOKEN &&
           this.peek(1).getTokenType() == TokenType.EQUALS_TOKEN)
        {
            Token identifierToken = this.nextToken();
            Token operatorToken = this.nextToken();
            Expression right = this.parseAssignmentExpression();
            return new AssignmentExpression(identifierToken, operatorToken, right);
        }
        return this.parseBinaryExpression();
    }

    private Expression parseBinaryExpression()
    {
        return this.parseBinaryExpression(0);
    }

    private Expression parseBinaryExpression(int parentPrecedence)
    {
        Expression left;
        int unaryOperatorPrecedence = SyntaxPrecedence.getUnaryOperatorPrecedence(this.currentToken().getTokenType());

        if(unaryOperatorPrecedence != 0 && unaryOperatorPrecedence >= parentPrecedence)
        {
            Token operatorToken = this.nextToken();
            Expression operand = this.parseBinaryExpression(unaryOperatorPrecedence);
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
            Expression right = this.parseBinaryExpression(binaryOperatorPrecedence);
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
            case NUMBER_TOKEN:
                return this.parseNumberExpression();
            case IDENTIFIER_KEYWORD_TOKEN:
                return this.parseNameExpression();
            default:
                return this.parseUnknownExpression();
        }
    }

    private Expression parseParenthesizedExpression()
    {
        Token left = this.nextToken();
        Expression expression = this.parseBinaryExpression();
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
        Token numberToken = this.nextToken();
        Object value = numberToken.getValue();
        return new LiteralExpression(numberToken, value);
    }

    private Expression parseNameExpression()
    {
        Token identifierToken = this.nextToken();
        return new NameExpression(identifierToken);
    }

    private Expression parseUnknownExpression()
    {
        Token token = this.currentToken();
        this.errorLog.add(SyntaxError.unexpectedToken(token.getSpan(), token.getTokenType()));
        return new LiteralExpression(token, null);
    }

    private Token matchTokens(TokenType type)
    {
        Token token = this.currentToken();
        if(token.getTokenType() == type)
            return this.nextToken();
        this.errorLog.add(SyntaxError.unexpectedTokenMatch(token.getSpan(), token.getTokenType(), type));
        return new Token(type, token.getPosition());
    }

    private Token peek(int offset)
    {
        int index = this.position + offset;
        if(index >= this.tokens.size())
            return this.tokens.get(this.tokens.size()-1);
        return this.tokens.get(index);
    }

    private Token nextToken()
    {
        Token token = this.currentToken();
        this.position++;
        return token;
    }

    private Token currentToken()
    {
        return this.peek(0);
    }
}
