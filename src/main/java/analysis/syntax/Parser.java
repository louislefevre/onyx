package analysis.syntax;

import analysis.lexical.Lexer;
import analysis.lexical.Token;
import errors.ErrorHandler;
import errors.SyntaxError;
import identifiers.TokenType;

import java.util.List;

public final class Parser
{
    private final List<Token> tokens;
    private final ErrorHandler errorHandler;
    private int position;

    public Parser(Lexer lexer, ErrorHandler errorHandler)
    {
        this.tokens = lexer.getTokens();
        this.errorHandler = errorHandler;
        this.position = 0;
    }

    public ParseTree getParseTree()
    {
        return new ParseTree(this.parseExpression());
    }

    private Expression parseExpression()
    {
        if (this.nextToken().getTokenType() != TokenType.EOF_TOKEN &&
            !ExpressionBinder.isBindable(this.currentToken(), this.nextToken()))
        {
            this.nextPosition();
            return this.parseUnknownExpression();
        }

        if (this.currentToken().getTokenType() == TokenType.IDENTIFIER_KEYWORD_TOKEN &&
            this.nextToken().getTokenType() == TokenType.EQUALS_TOKEN)
        {
            return this.parseAssignmentExpression();
        }
        return this.parseBinaryExpression();
    }

    private Expression parseAssignmentExpression()
    {
        Token identifierToken = this.currentTokenThenNext();
        Token operatorToken = this.currentTokenThenNext();
        Expression right = this.parseExpression();
        return new AssignmentExpression(identifierToken, operatorToken, right);
    }

    private Expression parseBinaryExpression()
    {
        return this.parseBinaryExpression(0);
    }

    private Expression parseBinaryExpression(int parentPrecedence)
    {
        Expression left = this.parseUnaryExpression(parentPrecedence);

        while (true)
        {
            int binaryOperatorPrecedence =
                    SyntaxPrecedence.getBinaryOperatorPrecedence(this.currentToken().getTokenType());
            if (binaryOperatorPrecedence == 0 || binaryOperatorPrecedence <= parentPrecedence)
                break;

            Token operatorToken = this.currentTokenThenNext();
            Expression right = this.parseBinaryExpression(binaryOperatorPrecedence);
            left = new BinaryExpression(left, operatorToken, right);
        }

        return left;
    }

    private Expression parseUnaryExpression(int parentPrecedence)
    {
        int unaryOperatorPrecedence = SyntaxPrecedence.getUnaryOperatorPrecedence(this.currentToken().getTokenType());

        if (unaryOperatorPrecedence != 0 && unaryOperatorPrecedence >= parentPrecedence)
        {
            Token operatorToken = this.currentTokenThenNext();
            Expression operand = this.parseBinaryExpression(unaryOperatorPrecedence);
            return new UnaryExpression(operatorToken, operand);
        }
        return this.parsePrimaryExpression();
    }

    private Expression parsePrimaryExpression()
    {
        switch (this.currentToken().getTokenType())
        {
            case OPEN_PARENTHESIS_TOKEN:
                return this.parseParenthesizedExpression();
            case FALSE_KEYWORD_TOKEN:
            case TRUE_KEYWORD_TOKEN:
                return this.parseBooleanExpression();
            case NUMBER_TOKEN:
                return this.parseNumberExpression();
            case STRING_TOKEN:
                return this.parseStringExpression();
            case IDENTIFIER_KEYWORD_TOKEN:
                return this.parseIdentifierExpression();
            default:
                return this.parseUnknownExpression();
        }
    }

    private Expression parseParenthesizedExpression()
    {
        Token left = this.currentTokenThenNext();
        Expression expression = this.parseExpression();
        Token right = this.currentTokenThenNext();

        if (right.getTokenType() != TokenType.CLOSE_PARENTHESIS_TOKEN)
            this.errorHandler.addError(SyntaxError.unexpectedTokenMatch(right.getSpan(), right.getTokenType(),
                                                                        TokenType.CLOSE_PARENTHESIS_TOKEN));

        return new ParenthesizedExpression(left, expression, right);
    }

    private Expression parseBooleanExpression()
    {
        Token keywordToken = this.currentTokenThenNext();
        boolean value = keywordToken.getTokenType() == TokenType.TRUE_KEYWORD_TOKEN;
        return new LiteralExpression(keywordToken, value);
    }

    private Expression parseNumberExpression()
    {
        Token numberToken = this.currentTokenThenNext();
        Object value = numberToken.getValue();
        return new LiteralExpression(numberToken, value);
    }

    private Expression parseStringExpression()
    {
        Token stringToken = this.currentTokenThenNext();
        Object value = stringToken.getValue();
        return new LiteralExpression(stringToken, value);
    }

    private Expression parseIdentifierExpression()
    {
        Token identifierToken = this.currentTokenThenNext();
        return new IdentifierExpression(identifierToken);
    }

    private Expression parseUnknownExpression()
    {
        Token token = this.currentTokenThenNext();
        this.errorHandler.addError(SyntaxError.unexpectedToken(token.getSpan(), token.getTokenType()));
        return new LiteralExpression(token, null);
    }

    private Token currentToken()
    {
        return this.peek(0);
    }

    private Token nextToken()
    {
        return this.peek(1);
    }

    private Token peek(int offset)
    {
        int index = this.position + offset;
        if (index >= this.tokens.size())
            return this.tokens.get(this.tokens.size() - 1);
        return this.tokens.get(index);
    }

    private void nextPosition()
    {
        this.position++;
    }

    private Token currentTokenThenNext()
    {
        Token token = this.currentToken();
        this.position++;
        return token;
    }
}
