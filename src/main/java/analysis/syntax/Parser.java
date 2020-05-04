package analysis.syntax;

import analysis.lexical.Lexer;
import analysis.lexical.Token;
import errors.ErrorHandler;
import errors.SyntaxError;
import identifiers.TokenType;
import lombok.Getter;
import symbols.SymbolTable;

import java.util.ArrayList;
import java.util.List;

import static errors.SyntaxError.unexpectedToken;
import static errors.SyntaxError.unexpectedTokenMatch;

public final class Parser
{
    private final List<Token> tokens;
    @Getter
    private final ErrorHandler errorHandler;
    @Getter
    private final SymbolTable symbolTable;
    private int position;

    public Parser(Lexer lexer)
    {
        this.tokens = lexer.getTokens();
        this.errorHandler = lexer.getErrorHandler();
        this.symbolTable = lexer.getSymbolTable();
        this.position = 0;
    }

    public ParseTree getParseTree()
    {
        return new ParseTree(parseStatement());
    }

    private Statement parseStatement()
    {
        switch (currentTokenType())
        {
            case OPEN_BRACE_TOKEN:
                return parseBlockStatement();
            case IF_TOKEN:
                return parseConditionalStatement();
            case LOOP_TOKEN:
                return parseLoopStatement();
            default:
                return parseExpressionStatement();
        }
    }

    private Statement parseBlockStatement()
    {
        Token openBrace = validateToken(TokenType.OPEN_BRACE_TOKEN);

        List<Statement> statements = new ArrayList<>();
        while (currentTokenType() != TokenType.EOF_TOKEN && currentTokenType() != TokenType.CLOSE_BRACE_TOKEN)
        {
            Statement statement = parseStatement();
            statements.add(statement);
        }

        Token closeBrace = validateToken(TokenType.CLOSE_BRACE_TOKEN);

        return new BlockStatement(openBrace, statements, closeBrace);
    }

    private Statement parseConditionalStatement()
    {
        Token ifToken = validateToken(TokenType.IF_TOKEN);
        Expression condition = parseExpression();
        Statement thenStatement = parseStatement();
        ConditionalStatement statement = new ConditionalStatement(ifToken, condition, thenStatement);

        if (currentTokenType() == TokenType.ELSE_TOKEN)
        {
            Token elseToken = validateToken(TokenType.ELSE_TOKEN);
            Statement elseStatement = parseStatement();
            statement.addElseStatement(elseToken, elseStatement);
        }

        return statement;
    }

    private Statement parseLoopStatement()
    {
        Token loopToken = validateToken(TokenType.LOOP_TOKEN);
        Token identifierToken = validateToken(TokenType.IDENTIFIER_TOKEN);
        Token equalsToken = validateToken(TokenType.EQUALS_TOKEN);
        Expression lowerBound = parseExpression();
        Token toToken = validateToken(TokenType.TO_TOKEN);
        Expression upperBound = parseExpression();
        Statement body = parseStatement();

        return new LoopStatement(loopToken, identifierToken, equalsToken, lowerBound, toToken, upperBound, body);
    }

    private ExpressionStatement parseExpressionStatement()
    {
        Expression expression = parseExpression();
        return new ExpressionStatement(expression);
    }

    private Expression parseExpression()
    {
        if (currentTokenType() == TokenType.IDENTIFIER_TOKEN)
        {
            switch (nextTokenType())
            {
                case EQUALS_TOKEN:
                    return parseAssignmentExpression(TokenType.EQUALS_TOKEN);
                case PLUS_EQUALS_TOKEN:
                    return parseAssignmentExpression(TokenType.PLUS_EQUALS_TOKEN);
                case MINUS_EQUALS_TOKEN:
                    return parseAssignmentExpression(TokenType.MINUS_EQUALS_TOKEN);
                case STAR_EQUALS_TOKEN:
                    return parseAssignmentExpression(TokenType.STAR_EQUALS_TOKEN);
                case SLASH_EQUALS_TOKEN:
                    return parseAssignmentExpression(TokenType.SLASH_EQUALS_TOKEN);
                case PERCENT_EQUALS_TOKEN:
                    return parseAssignmentExpression(TokenType.PERCENT_EQUALS_TOKEN);
                case CARET_EQUALS_TOKEN:
                    return parseAssignmentExpression(TokenType.CARET_EQUALS_TOKEN);
            }
        }

        return parseBinaryExpression(0);
    }

    private Expression parseAssignmentExpression(TokenType tokenType)
    {
        Token identifierToken = validateToken(TokenType.IDENTIFIER_TOKEN);
        Token assignmentToken = validateToken(tokenType);
        Expression expression = parseExpression();

        return new AssignmentExpression(identifierToken, assignmentToken, expression);
    }

    private Expression parseBinaryExpression(int parentPrecedence)
    {
        if (ExpressionBinder.tokensNotBindable(currentToken(), nextToken()))
        {
            //nextPosition();
            //return parseUnknownExpression();
        }

        Expression leftOperand = parseUnaryExpression(parentPrecedence);

        while (true)
        {
            int operatorPrecedence = OperatorPrecedence.getBinaryOperatorPrecedence(currentToken().getType());

            if (operatorPrecedence == 0 || operatorPrecedence <= parentPrecedence)
                break;

            Token operatorToken = currentTokenThenNext();
            Expression rightOperand = parseBinaryExpression(operatorPrecedence);
            leftOperand = new BinaryExpression(leftOperand, operatorToken, rightOperand);
        }

        return leftOperand;
    }

    private Expression parseUnaryExpression(int parentPrecedence)
    {
        int operatorPrecedence = OperatorPrecedence.getUnaryOperatorPrecedence(currentToken().getType());

        if (operatorPrecedence != 0 && operatorPrecedence >= parentPrecedence)
        {
            Token operatorToken = currentTokenThenNext();
            Expression operand = parseBinaryExpression(operatorPrecedence);
            return new UnaryExpression(operatorToken, operand);
        }

        return parsePrimaryExpression();
    }

    private Expression parsePrimaryExpression()
    {
        switch (currentToken().getType())
        {
            case INTEGER_TOKEN:
            case DOUBLE_TOKEN:
            case BOOLEAN_TOKEN:
            case STRING_TOKEN:
                return parseLiteralExpression();
            case IDENTIFIER_TOKEN:
                return parseIdentifierExpression();
            case OPEN_PARENTHESIS_TOKEN:
                return parseParenthesizedExpression();
            default:
                return parseUnknownExpression();
        }
    }

    private Expression parseLiteralExpression()
    {
        Token literalToken = currentTokenThenNext();
        Object value = literalToken.getValue();
        return new LiteralExpression(literalToken, value);
    }

    private Expression parseIdentifierExpression()
    {
        Token identifierToken = validateToken(TokenType.IDENTIFIER_TOKEN);
        return new IdentifierExpression(identifierToken);
    }

    private Expression parseParenthesizedExpression()
    {
        Token openParenthesisToken = validateToken(TokenType.OPEN_PARENTHESIS_TOKEN);
        Expression expression = parseExpression();
        Token closeParenthesisToken = validateToken(TokenType.CLOSE_PARENTHESIS_TOKEN);

        return new ParenthesizedExpression(openParenthesisToken, expression, closeParenthesisToken);
    }

    private Expression parseUnknownExpression()
    {
        Token currentToken = currentTokenThenNext();
        Token placeholderToken = new Token(TokenType.BAD_TOKEN, currentToken.getSyntax(),
                                           currentToken.getValue(), currentToken.getPosition());

        SyntaxError error = unexpectedToken(currentToken.getSpan(), currentToken.getType());
        errorHandler.addError(error);

        return new LiteralExpression(placeholderToken, null);
    }

    private Token validateToken(TokenType type)
    {
        Token currentToken = currentToken();
        if (currentToken.getType() == type)
            return currentTokenThenNext();

        SyntaxError error = unexpectedTokenMatch(currentToken.getSpan(), currentToken.getType(), type);
        errorHandler.addError(error);

        return new Token(TokenType.BAD_TOKEN, currentToken.getSyntax(), currentToken.getValue(), currentToken.getPosition());
    }

    private TokenType currentTokenType()
    {
        return currentToken().getType();
    }

    private TokenType nextTokenType()
    {
        return nextToken().getType();
    }

    private Token currentToken()
    {
        return peek(0);
    }

    private Token nextToken()
    {
        return peek(1);
    }

    private Token peek(int offset)
    {
        int index = position + offset;
        if (index >= tokens.size())
            return tokens.get(tokens.size() - 1);
        return tokens.get(index);
    }

    private void nextPosition()
    {
        position++;
    }

    private Token currentTokenThenNext()
    {
        Token token = currentToken();
        position++;
        return token;
    }
}
