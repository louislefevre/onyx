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

import static errors.SyntaxError.invalidToken;
import static errors.SyntaxError.invalidTokenPair;
import static identifiers.TokenType.*;

public final class Parser
{
    private final List<Token> tokens;
    @Getter private final ErrorHandler errorHandler;
    @Getter private final SymbolTable symbolTable;
    @Getter private final boolean replMode;
    private int position;

    public Parser(Lexer lexer)
    {
        this.tokens = lexer.getTokens();
        this.errorHandler = lexer.getErrorHandler();
        this.symbolTable = lexer.getSymbolTable();
        this.replMode = lexer.isReplMode();
        this.position = 0;
    }

    public ParseTree getParseTree()
    {
        Statement statement = replMode ? replTree() : ideTree();
        return new ParseTree(statement);
    }

    private Statement replTree()
    {
        Expression expression = parseExpression();
        Statement statement = new ExpressionStatement(expression);
        Token endToken = validateToken(EOF_TOKEN);
        return new SourceStatement(statement, endToken);
    }

    private Statement ideTree()
    {
        List<Statement> statements = new ArrayList<>();
        while (currentTokenType() != EOF_TOKEN)
        {
            if (currentTokenType() == LINE_BREAK_TOKEN)
            {
                position++;
                continue;
            }

            Statement statement = parseStatement();
            statements.add(statement);
        }
        Token endToken = validateToken(EOF_TOKEN);

        return new SourceStatement(statements, endToken);
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

    private BlockStatement parseBlockStatement()
    {
        Token openBrace = validateToken(OPEN_BRACE_TOKEN);
        validateToken(LINE_BREAK_TOKEN);

        List<Statement> statements = new ArrayList<>();
        while (currentTokenType() != EOF_TOKEN && currentTokenType() != CLOSE_BRACE_TOKEN)
        {
            if (currentTokenType() == LINE_BREAK_TOKEN)
            {
                position++;
                continue;
            }

            Statement statement = parseStatement();
            statements.add(statement);
        }

        Token closeBrace = validateToken(CLOSE_BRACE_TOKEN);
        validateToken(LINE_BREAK_TOKEN);

        return new BlockStatement(openBrace, statements, closeBrace);
    }

    private ConditionalStatement parseConditionalStatement()
    {
        Token ifToken = validateToken(IF_TOKEN);
        Expression condition = parseExpression();
        validateToken(LINE_BREAK_TOKEN);
        Statement thenStatement = parseStatement();
        ConditionalStatement statement = new ConditionalStatement(ifToken, condition, thenStatement);

        if (currentTokenType() == ELSE_TOKEN)
        {
            Token elseToken = validateToken(ELSE_TOKEN);
            validateToken(LINE_BREAK_TOKEN);
            Statement elseStatement = parseStatement();
            statement.addElseStatement(elseToken, elseStatement);
        }

        return statement;
    }

    private LoopStatement parseLoopStatement()
    {
        Token loopToken = validateToken(LOOP_TOKEN);
        Expression lowerBound = parseExpression();
        Token toToken = validateToken(TO_TOKEN);
        Expression upperBound = parseExpression();
        validateToken(LINE_BREAK_TOKEN);
        Statement body = parseStatement();

        return new LoopStatement(loopToken, lowerBound, toToken, upperBound, body);
    }

    private ExpressionStatement parseExpressionStatement()
    {
        Expression expression = parseExpression();
        validateToken(LINE_BREAK_TOKEN);
        return new ExpressionStatement(expression);
    }

    private Expression parseExpression()
    {
        if (currentTokenType() == IDENTIFIER_TOKEN)
        {
            switch (nextTokenType())
            {
                case EQUALS_TOKEN:
                    return parseAssignmentExpression(EQUALS_TOKEN);
                case PLUS_EQUALS_TOKEN:
                    return parseAssignmentExpression(PLUS_EQUALS_TOKEN);
                case MINUS_EQUALS_TOKEN:
                    return parseAssignmentExpression(MINUS_EQUALS_TOKEN);
                case STAR_EQUALS_TOKEN:
                    return parseAssignmentExpression(STAR_EQUALS_TOKEN);
                case SLASH_EQUALS_TOKEN:
                    return parseAssignmentExpression(SLASH_EQUALS_TOKEN);
                case PERCENT_EQUALS_TOKEN:
                    return parseAssignmentExpression(PERCENT_EQUALS_TOKEN);
                case CARET_EQUALS_TOKEN:
                    return parseAssignmentExpression(CARET_EQUALS_TOKEN);
            }
        }

        return parseBinaryExpression(0);
    }

    private AssignmentExpression parseAssignmentExpression(TokenType tokenType)
    {
        IdentifierExpression identifier = parseIdentifierExpression();
        Token assignmentToken = validateToken(tokenType);
        Expression assignment = parseExpression();

        return new AssignmentExpression(identifier, assignmentToken, assignment);
    }

    private Expression parseBinaryExpression(int parentPrecedence)
    {
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

    private LiteralExpression parseLiteralExpression()
    {
        Token literalToken = currentTokenThenNext();
        Object value = literalToken.getValue();
        return new LiteralExpression(literalToken, value);
    }

    private IdentifierExpression parseIdentifierExpression()
    {
        Token identifierToken = validateToken(IDENTIFIER_TOKEN);
        return new IdentifierExpression(identifierToken);
    }

    private ParenthesizedExpression parseParenthesizedExpression()
    {
        Token openParenthesisToken = validateToken(OPEN_PARENTHESIS_TOKEN);
        Expression expression = parseExpression();
        Token closeParenthesisToken = validateToken(CLOSE_PARENTHESIS_TOKEN);

        return new ParenthesizedExpression(openParenthesisToken, expression, closeParenthesisToken);
    }

    private LiteralExpression parseUnknownExpression()
    {
        Token currentToken = currentTokenThenNext();
        Token placeholderToken = new Token(BAD_TOKEN, currentToken.getSyntax(),
                                           currentToken.getValue(), currentToken.getPosition());

        SyntaxError error = invalidToken(currentToken.getSpan(), currentToken.getType());
        errorHandler.addError(error);

        return new LiteralExpression(placeholderToken, null);
    }

    private Token validateToken(TokenType type)
    {
        Token currentToken = currentToken();
        if (currentToken.getType() == type)
            return currentTokenThenNext();

        SyntaxError error = invalidTokenPair(currentToken.getSpan(), currentToken.getType(), type);
        errorHandler.addError(error);

        return new Token(BAD_TOKEN, currentToken.getSyntax(), currentToken.getValue(), currentToken.getPosition());
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

    private Token currentTokenThenNext()
    {
        Token token = currentToken();
        position++;
        return token;
    }
}
