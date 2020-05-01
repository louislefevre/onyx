package analysis.syntax;

import analysis.lexical.Lexer;
import analysis.lexical.Token;
import errors.ErrorHandler;
import errors.SyntaxError;
import identifiers.TokenType;
import symbols.SymbolTable;

import java.util.ArrayList;
import java.util.List;

public final class Parser
{
    private final List<Token> tokens;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;
    private int position;

    public Parser(Lexer lexer)
    {
        this.tokens = lexer.getTokens();
        this.errorHandler = lexer.getErrorHandler();
        this.symbolTable = lexer.getSymbolTable();
        this.position = 0;
    }

    public ErrorHandler getErrorHandler()
    {
        return this.errorHandler;
    }

    public SymbolTable getSymbolTable()
    {
        return this.symbolTable;
    }

    public ParseTree getParseTree()
    {
        return new ParseTree(this.parseStatement());
    }

    private Statement parseStatement()
    {
        if (this.currentToken().getTokenType() == TokenType.OPEN_BRACE_TOKEN)
            return this.parseBlockStatement();
        return this.parseExpressionStatement();
    }

    private Statement parseBlockStatement()
    {
        List<Statement> statements = new ArrayList<>();

        Token openBrace = this.validateToken(TokenType.OPEN_BRACE_TOKEN);
        while (this.currentToken().getTokenType() != TokenType.EOF_TOKEN &&
               this.currentToken().getTokenType() != TokenType.CLOSE_BRACE_TOKEN)
        {
            Statement statement = this.parseStatement();
            statements.add(statement);
        }
        Token closeBrace = this.validateToken(TokenType.CLOSE_BRACE_TOKEN);

        return new BlockStatement(openBrace, statements, closeBrace);
    }

    private ExpressionStatement parseExpressionStatement()
    {
        Expression expression = this.parseExpression();
        return new ExpressionStatement(expression);
    }

    private Expression parseExpression()
    {
        TokenType currentTokenType = this.currentToken().getTokenType();
        TokenType nextTokenType = this.nextToken().getTokenType();

        if (currentTokenType == TokenType.IDENTIFIER_TOKEN)
        {
            switch (nextTokenType)
            {
                case EQUALS_TOKEN:
                    return this.parseAssignmentExpression(TokenType.EQUALS_TOKEN);
                case PLUS_EQUALS_TOKEN:
                    return this.parseAssignmentExpression(TokenType.PLUS_EQUALS_TOKEN);
                case MINUS_EQUALS_TOKEN:
                    return this.parseAssignmentExpression(TokenType.MINUS_EQUALS_TOKEN);
                case STAR_EQUALS_TOKEN:
                    return this.parseAssignmentExpression(TokenType.STAR_EQUALS_TOKEN);
                case SLASH_EQUALS_TOKEN:
                    return this.parseAssignmentExpression(TokenType.SLASH_EQUALS_TOKEN);
                case PERCENT_EQUALS_TOKEN:
                    return this.parseAssignmentExpression(TokenType.PERCENT_EQUALS_TOKEN);
                case CARET_EQUALS_TOKEN:
                    return this.parseAssignmentExpression(TokenType.CARET_EQUALS_TOKEN);
            }
        }

        return this.parseBinaryExpression(0);
    }

    private Expression parseAssignmentExpression(TokenType tokenType)
    {
        Token identifierToken = this.validateToken(TokenType.IDENTIFIER_TOKEN);
        Token assignmentToken = this.validateToken(tokenType);
        Expression expression = this.parseExpression();
        return new AssignmentExpression(identifierToken, assignmentToken, expression);
    }

    private Expression parseBinaryExpression(int parentPrecedence)
    {
        if (ExpressionBinder.tokensNotBindable(this.currentToken(), this.nextToken()))
        {
            this.nextPosition();
            return this.parseUnknownExpression();
        }

        Expression leftOperand = this.parseUnaryExpression(parentPrecedence);

        while (true)
        {
            int binaryOperatorPrecedence =
                    OperatorPrecedence.getBinaryOperatorPrecedence(this.currentToken().getTokenType());
            if (binaryOperatorPrecedence == 0 || binaryOperatorPrecedence <= parentPrecedence)
                break;

            Token operatorToken = this.currentTokenThenNext();
            Expression rightOperand = this.parseBinaryExpression(binaryOperatorPrecedence);
            leftOperand = new BinaryExpression(leftOperand, operatorToken, rightOperand);
        }

        return leftOperand;
    }

    private Expression parseUnaryExpression(int parentPrecedence)
    {
        int unaryOperatorPrecedence = OperatorPrecedence.getUnaryOperatorPrecedence(this.currentToken().getTokenType());

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
            case FALSE_KEYWORD_TOKEN:
            case TRUE_KEYWORD_TOKEN:
            case INTEGER_TOKEN:
            case DOUBLE_TOKEN:
            case STRING_TOKEN:
                return this.parseLiteralExpression();
            case IDENTIFIER_TOKEN:
                return this.parseIdentifierExpression();
            case OPEN_PARENTHESIS_TOKEN:
                return this.parseParenthesizedExpression();
            default:
                return this.parseUnknownExpression();
        }
    }

    private Expression parseLiteralExpression()
    {
        Token literalToken = this.currentTokenThenNext();
        Object value = literalToken.getValue();
        return new LiteralExpression(literalToken, value);
    }

    private Expression parseIdentifierExpression()
    {
        Token identifierToken = this.validateToken(TokenType.IDENTIFIER_TOKEN);
        return new IdentifierExpression(identifierToken);
    }

    private Expression parseParenthesizedExpression()
    {
        Token openParenthesisToken = this.validateToken(TokenType.OPEN_PARENTHESIS_TOKEN);
        Expression expression = this.parseExpression();
        Token closeParenthesisToken = this.validateToken(TokenType.CLOSE_PARENTHESIS_TOKEN);
        return new ParenthesizedExpression(openParenthesisToken, expression, closeParenthesisToken);
    }

    private Expression parseUnknownExpression()
    {
        Token currentToken = this.currentTokenThenNext();
        Token placeholderToken = new Token(TokenType.BAD_TOKEN, currentToken.getSyntax(), currentToken.getValue(),
                                           currentToken.getPosition());
        this.errorHandler.addError(SyntaxError.unexpectedToken(currentToken.getSpan(), currentToken.getTokenType()));
        return new LiteralExpression(placeholderToken, null);
    }

    private Token validateToken(TokenType type)
    {
        Token currentToken = this.currentToken();
        if (currentToken.getTokenType() == type)
            return this.currentTokenThenNext();
        this.errorHandler.addError(SyntaxError.unexpectedTokenMatch(currentToken.getSpan(),
                                                                    currentToken.getTokenType(), type));
        return new Token(TokenType.BAD_TOKEN, currentToken.getSyntax(), currentToken.getValue(),
                         currentToken.getPosition());
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
