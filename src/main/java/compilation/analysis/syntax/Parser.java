package compilation.analysis.syntax;

import compilation.analysis.lexical.Lexer;
import compilation.analysis.lexical.Token;
import errors.ErrorHandler;
import errors.SyntaxError;
import types.TokenType;

import java.util.ArrayList;
import java.util.List;

import static errors.SyntaxError.emptyParenthesis;
import static errors.SyntaxError.incompleteExpression;
import static errors.SyntaxError.invalidStatement;
import static errors.SyntaxError.unexpectedToken;
import static types.TokenType.*;

/**
 * The Parser class is responsible for performing syntax analysis on a List of Tokens, discovering whether or not
 * the structure of the program conforms to the rules of the source languages grammar.
 * <p>
 * It creates a ParseTree that holds the Statements and Expressions which make up the structure of the input code.
 * During this stage any Expressions that are unrecognised by the source language are identified and reported
 * as an error.
 * <p>
 * The List of Tokens produced by the Lexer is retrieved, with its contents being parsed sequentially. Each individual
 * Token is inspected and recursively analysed in an attempt to discover the structure of its parent expression
 * or statement, with the result being added to a List of Statements held by a SourceStatement object. This
 * represents the entire program.
 * <p>
 * Any compilation errors that occur during this stage are passed to the ErrorHandler in the form of SyntaxError
 * objects.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class Parser
{
    private final List<Token> tokens;
    private final ErrorHandler errorHandler;
    private final boolean replMode;
    private int position;

    /**
     * Constructs a Parser object initialised with the tokens from the Lexer.
     * <p>
     * The Lexer always begins at position 0 in List of Tokens, and adds any errors to the errorHandler.
     * <p>
     * If REPL mode is true, the Parser will only parse a single line and disallow the use of multiline
     * Statements (e.g. conditionals, loops, blocks). If REPL mode is false, the Parser will parse multiple
     * lines and allow the use of multiline Statements. In summary, the former is for single-line parsing,
     * whilst the latter is for multiline parsing.
     *
     * @param lexer The Lexer used to generate the List of Tokens
     * @param errorHandler The ErrorHandler to store any errors that occur
     * @param replMode The boolean indicating the Parser should run in REPL mode
     */
    public Parser(Lexer lexer, ErrorHandler errorHandler, boolean replMode)
    {
        this.tokens = lexer.getTokens();
        this.errorHandler = errorHandler;
        this.replMode = replMode;
        this.position = 0;
    }

    /**
     * Returns a ParseTree object generated from the Lexer.
     * <p>
     * The List of Token objects is retrieved from the Lexer, parsed into a series of Statements and
     * Expressions that represent the structure of the input code, and then returned in the form
     * of a ParseTree.
     *
     * @return A ParseTree containing the root Statement
     */
    public ParseTree getParseTree()
    {
        Statement statement = replMode ? replTree() : ideTree();
        return new ParseTree(statement);
    }

    private Statement replTree()
    {
        Statement statement = parseExpressionStatement();
        Token endToken = parseToken(EOF_TOKEN);
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
        Token endToken = parseToken(EOF_TOKEN);

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
            case IDENTIFIER_TOKEN:
                return parseExpressionStatement();
            default:
                ExpressionStatement statement = parseExpressionStatement();
                SyntaxError error = invalidStatement(statement.getSpan());
                errorHandler.add(error);
                return statement;
        }
    }

    private BlockStatement parseBlockStatement()
    {
        Token openBrace = parseToken(OPEN_BRACE_TOKEN);
        parseToken(LINE_BREAK_TOKEN);

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

        Token closeBrace = parseToken(CLOSE_BRACE_TOKEN);
        parseToken(LINE_BREAK_TOKEN);

        return new BlockStatement(openBrace, statements, closeBrace);
    }

    private ConditionalStatement parseConditionalStatement()
    {
        Token ifToken = parseToken(IF_TOKEN);
        Expression condition = parseExpression();
        parseToken(LINE_BREAK_TOKEN);
        Statement thenStatement = parseStatement();
        ConditionalStatement statement = new ConditionalStatement(ifToken, condition, thenStatement);

        if (currentTokenType() == ELSE_TOKEN)
        {
            Token elseToken = parseToken(ELSE_TOKEN);
            parseToken(LINE_BREAK_TOKEN);
            Statement elseStatement = parseStatement();
            statement.addElseStatement(elseToken, elseStatement);
        }

        return statement;
    }

    private LoopStatement parseLoopStatement()
    {
        Token loopToken = parseToken(LOOP_TOKEN);
        Expression lowerBound = parseExpression();
        Token toToken = parseToken(TO_TOKEN);
        Expression upperBound = parseExpression();
        parseToken(LINE_BREAK_TOKEN);
        Statement body = parseStatement();

        return new LoopStatement(loopToken, lowerBound, toToken, upperBound, body);
    }

    private ExpressionStatement parseExpressionStatement()
    {
        Expression expression = parseExpression();
        parseToken(LINE_BREAK_TOKEN);
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
        Token assignmentToken = parseToken(tokenType);
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
        Token identifierToken = parseToken(IDENTIFIER_TOKEN);
        return new IdentifierExpression(identifierToken);
    }

    private ParenthesizedExpression parseParenthesizedExpression()
    {
        Token openParenthesisToken = parseToken(OPEN_PARENTHESIS_TOKEN);

        if (currentTokenType() == CLOSE_PARENTHESIS_TOKEN)
        {
            SyntaxError error = emptyParenthesis(openParenthesisToken.getSpan(), currentToken().getSpan());
            errorHandler.add(error);
        }

        Expression expression = parseExpression();
        Token closeParenthesisToken = parseToken(CLOSE_PARENTHESIS_TOKEN);

        return new ParenthesizedExpression(openParenthesisToken, expression, closeParenthesisToken);
    }

    private LiteralExpression parseUnknownExpression()
    {
        Token currentToken = currentTokenThenNext();
        Token badToken = new Token(BAD_TOKEN, currentToken.getSyntax(),
                                   currentToken.getValue(), currentToken.getPosition());

        SyntaxError error = incompleteExpression(currentToken.getSpan());
        errorHandler.add(error);

        return new LiteralExpression(badToken, null);
    }

    private Token parseToken(TokenType type)
    {
        if (isCurrentTokenType(type))
            return currentTokenThenNext();

        Token currentToken = currentToken();
        return new Token(BAD_TOKEN, currentToken.getSyntax(), currentToken.getValue(), currentToken.getPosition());
    }

    private boolean isCurrentTokenType(TokenType type)
    {
        Token currentToken = currentToken();
        if (currentToken.getType() == type)
            return true;

        SyntaxError error = unexpectedToken(currentToken, type);
        errorHandler.add(error);
        return false;
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
