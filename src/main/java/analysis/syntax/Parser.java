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
        return symbolTable;
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
        if (this.currentToken().getTokenType() == TokenType.IDENTIFIER_TOKEN &&
            this.nextToken().getTokenType() == TokenType.EQUALS_TOKEN)
            return this.parseAssignmentExpression();

        return this.parseBinaryExpression(0);
    }

    private Expression parseAssignmentExpression()
    {
        Token identifierToken = this.validateToken(TokenType.IDENTIFIER_TOKEN);
        Token equalsToken = this.validateToken(TokenType.EQUALS_TOKEN);
        Expression right = this.parseExpression();
        return new AssignmentExpression(identifierToken, equalsToken, right);
    }

    private Expression parseBinaryExpression(int parentPrecedence)
    {
        if (ExpressionBinder.tokensNotBindable(this.currentToken(), this.nextToken()))
        {
            this.nextPosition();
            return this.parseUnknownExpression();
        }

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
        Token token = this.currentTokenThenNext();
        Object value = token.getValue();
        return new LiteralExpression(token, value);
    }

    private Expression parseIdentifierExpression()
    {
        Token identifierToken = this.validateToken(TokenType.IDENTIFIER_TOKEN);
        return new IdentifierExpression(identifierToken);
    }

    private Expression parseParenthesizedExpression()
    {
        Token openParenthesis = this.validateToken(TokenType.OPEN_PARENTHESIS_TOKEN);
        Expression expression = this.parseExpression();
        Token closeParenthesis = this.validateToken(TokenType.CLOSE_PARENTHESIS_TOKEN);
        return new ParenthesizedExpression(openParenthesis, expression, closeParenthesis);
    }

    private Expression parseUnknownExpression()
    {
        Token current = this.currentTokenThenNext();
        this.errorHandler.addError(SyntaxError.unexpectedToken(current.getSpan(), current.getTokenType()));
        Token placeholder = new Token(TokenType.BAD_TOKEN, current.getSyntax(), current.getValue(),
                                      current.getPosition());
        return new LiteralExpression(placeholder, null);
    }

    private Token validateToken(TokenType type)
    {
        Token current = this.currentToken();
        if (current.getTokenType() == type)
            return this.currentTokenThenNext();
        this.errorHandler.addError(SyntaxError.unexpectedTokenMatch(current.getSpan(), current.getTokenType(), type));
        return new Token(TokenType.BAD_TOKEN, current.getSyntax(), current.getValue(), current.getPosition());
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
