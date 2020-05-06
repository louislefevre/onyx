package analysis.semantic;

import analysis.lexical.Token;
import analysis.syntax.*;
import errors.ErrorHandler;
import errors.SemanticError;
import identifiers.ObjectType;
import identifiers.TokenType;
import lombok.Getter;
import symbols.Symbol;
import symbols.SymbolTable;

import java.util.ArrayList;
import java.util.List;

public final class TypeChecker
{
    private final ParseTree parseTree;
    @Getter
    private final ErrorHandler errorHandler;
    @Getter
    private final SymbolTable symbolTable;
    @Getter
    private final boolean replMode;

    public TypeChecker(Parser parser)
    {
        this.parseTree = parser.getParseTree();
        this.errorHandler = parser.getErrorHandler();
        this.symbolTable = parser.getSymbolTable();
        this.replMode = parser.isReplMode();
    }

    public AnnotatedParseTree getAnnotatedParseTree()
    {
        return new AnnotatedParseTree(getAnnotatedStatement());
    }

    private AnnotatedStatement getAnnotatedStatement()
    {
        try
        {
            return annotateStatement(parseTree.getStatement());
        }
        catch (Exception exception)
        {
            String errorMessage = SemanticError.exceptionOccurred(exception);
            System.out.println(errorMessage);
            return null;
        }
    }

    private AnnotatedStatement annotateStatement(Statement statement) throws Exception
    {
        switch (statement.getStatementType())
        {
            case SOURCE_STATEMENT:
                return annotatedSourceStatement((SourceStatement) statement);
            case EXPRESSION_STATEMENT:
                return annotateExpressionStatement((ExpressionStatement) statement);
            case BLOCK_STATEMENT:
                return annotateBlockStatement((BlockStatement) statement);
            case CONDITIONAL_STATEMENT:
                return annotateConditionalStatement((ConditionalStatement) statement);
            case LOOP_STATEMENT:
                return annotateLoopStatement((LoopStatement) statement);
            default:
                String errorMessage = SemanticError.undefinedStatement(statement.getStatementType().toString());
                throw new Exception(errorMessage);
        }
    }

    private AnnotatedStatement annotatedSourceStatement(SourceStatement sourceStatement) throws Exception
    {
        List<AnnotatedStatement> statements = new ArrayList<>();

        for (Statement statement : sourceStatement.getStatements())
        {
            AnnotatedStatement annotatedStatement = annotateStatement(statement);
            statements.add(annotatedStatement);
        }

        return new AnnotatedSourceStatement(statements);
    }

    private AnnotatedStatement annotateExpressionStatement(ExpressionStatement expressionStatement) throws Exception
    {
        Expression expression = expressionStatement.getExpression();
        AnnotatedExpression annotatedExpression = annotateExpression(expression);

        return new AnnotatedExpressionStatement(annotatedExpression);
    }

    private AnnotatedStatement annotateBlockStatement(BlockStatement blockStatement) throws Exception
    {
        List<AnnotatedStatement> statements = new ArrayList<>();

        for (Statement statement : blockStatement.getStatements())
        {
            AnnotatedStatement annotatedStatement = annotateStatement(statement);
            statements.add(annotatedStatement);
        }

        return new AnnotatedBlockStatement(statements);
    }

    private AnnotatedStatement annotateConditionalStatement(ConditionalStatement conditionalStatement) throws Exception
    {
        AnnotatedExpression condition = annotateExpression(conditionalStatement.getCondition());

        if (condition.getObjectType() != ObjectType.BOOLEAN_OBJECT)
        {
            SemanticError error = SemanticError.invalidConditionalTypes(conditionalStatement.getIfToken().getSpan(),
                                                                        condition.getObjectType(),
                                                                        ObjectType.BOOLEAN_OBJECT);
            errorHandler.addError(error);
        }

        AnnotatedStatement thenStatement = annotateStatement(conditionalStatement.getThenStatement());

        AnnotatedStatement elseStatement;
        if (conditionalStatement.includesElseStatement())
            elseStatement = annotateStatement(conditionalStatement.getElseStatement().getStatement());
        else
            elseStatement = null;

        return new AnnotatedConditionalStatement(condition, thenStatement, elseStatement);
    }

    private AnnotatedStatement annotateLoopStatement(LoopStatement loopStatement) throws Exception
    {
        String name = loopStatement.getIdentifierToken().getSyntax();
        AnnotatedExpression lowerBound = annotateExpression(loopStatement.getLowerBound());
        AnnotatedExpression upperBound = annotateExpression(loopStatement.getUpperBound());

        if (lowerBound.getObjectType() != ObjectType.INTEGER_OBJECT)
        {
            SemanticError error = SemanticError.invalidConditionalTypes(loopStatement.getLoopToken().getSpan(),
                                                                        lowerBound.getObjectType(),
                                                                        ObjectType.INTEGER_OBJECT);
            errorHandler.addError(error);
        }
        if (upperBound.getObjectType() != ObjectType.INTEGER_OBJECT)
        {
            SemanticError error = SemanticError.invalidConditionalTypes(loopStatement.getLoopToken().getSpan(),
                                                                        upperBound.getObjectType(),
                                                                        ObjectType.INTEGER_OBJECT);
            errorHandler.addError(error);
        }

        Symbol symbol = new Symbol(name, null, ObjectType.INTEGER_OBJECT);
        symbolTable.addSymbol(symbol);

        AnnotatedStatement body = annotateStatement(loopStatement.getBody());

        return new AnnotatedLoopStatement(symbol, lowerBound, upperBound, body);
    }

    private AnnotatedExpression annotateExpression(Expression expression) throws Exception
    {
        switch (expression.getExpressionType())
        {
            case LITERAL_EXPRESSION:
                return annotateLiteralExpression((LiteralExpression) expression);
            case UNARY_EXPRESSION:
                return annotateUnaryExpression((UnaryExpression) expression);
            case BINARY_EXPRESSION:
                return annotateBinaryExpression((BinaryExpression) expression);
            case IDENTIFIER_EXPRESSION:
                return annotateIdentifierExpression((IdentifierExpression) expression);
            case ASSIGNMENT_EXPRESSION:
                return annotateAssignmentExpression((AssignmentExpression) expression);
            case PARENTHESIZED_EXPRESSION:
                return annotateParenthesizedExpression((ParenthesizedExpression) expression);
            default:
                String errorMessage = SemanticError.undefinedExpression(expression.getExpressionType().toString());
                throw new Exception(errorMessage);
        }
    }

    private AnnotatedExpression annotateParenthesizedExpression(ParenthesizedExpression parenthesizedExpression) throws Exception
    {
        Expression expression = parenthesizedExpression.getExpression();
        return annotateExpression(expression);
    }

    private AnnotatedExpression annotateLiteralExpression(LiteralExpression literalExpression)
    {
        Object value = literalExpression.getValue();
        return new AnnotatedLiteralExpression(value);
    }

    private AnnotatedExpression annotateUnaryExpression(UnaryExpression unaryExpression) throws Exception
    {
        AnnotatedExpression operand = annotateExpression(unaryExpression.getOperand());
        AnnotatedUnaryOperator operator = TypeBinder.bindUnaryOperators(unaryExpression.getOperatorToken().getType(),
                                                                        operand.getObjectType());

        if (operator == null)
        {
            SemanticError error = SemanticError.undefinedUnaryOperator(unaryExpression.getOperatorToken().getSpan(),
                                                                       unaryExpression.getOperatorToken().getSyntax(),
                                                                       operand.getObjectType());
            errorHandler.addError(error);
            return new AnnotatedLiteralExpression(null);
        }

        return new AnnotatedUnaryExpression(operator, operand);
    }

    private AnnotatedExpression annotateBinaryExpression(BinaryExpression binaryExpression) throws Exception
    {
        AnnotatedExpression leftOperand = annotateExpression(binaryExpression.getLeftOperand());
        AnnotatedExpression rightOperand = annotateExpression(binaryExpression.getRightOperand());
        AnnotatedBinaryOperator operator = TypeBinder.bindBinaryOperators(binaryExpression.getOperatorToken().getType(),
                                                                          leftOperand.getObjectType(),
                                                                          rightOperand.getObjectType());

        if (operator == null)
        {
            errorHandler.addError(SemanticError.undefinedBinaryOperator(binaryExpression.getOperatorToken().getSpan(),
                                                                        binaryExpression.getOperatorToken().getSyntax(),
                                                                        leftOperand.getObjectType(),
                                                                        rightOperand.getObjectType()));
            return new AnnotatedLiteralExpression(null);
        }

        return new AnnotatedBinaryExpression(leftOperand, operator, rightOperand);
    }

    private AnnotatedExpression annotateIdentifierExpression(IdentifierExpression identifierExpression)
    {
        String name = identifierExpression.getIdentifierToken().getSyntax();

        if (!symbolTable.containsSymbol(name))
        {
            SemanticError error = SemanticError.undefinedIdentifier(identifierExpression.getIdentifierToken().getSpan(), name);
            errorHandler.addError(error);
            return new AnnotatedLiteralExpression(null);
        }
        ObjectType type = symbolTable.getSymbol(name).getType();

        return new AnnotatedIdentifierExpression(name, type);
    }

    private AnnotatedExpression annotateAssignmentExpression(AssignmentExpression assignmentExpression) throws Exception
    {
        String name = assignmentExpression.getIdentifierExpression().getIdentifierToken().getSyntax();
        AnnotatedExpression expression = annotateExpression(assignmentExpression.getExpression());
        Token assignmentToken = assignmentExpression.getAssignmentToken();
        TokenType assignmentTokenType = assignmentToken.getType();
        ObjectType symbolType = symbolTable.containsSymbol(name) ? symbolTable.getSymbol(name).getType() : ObjectType.NULL_OBJECT;
        ObjectType assignmentType = expression.getObjectType();
        AnnotatedAssignmentOperator operator = TypeBinder.bindAssignmentOperators(assignmentTokenType, symbolType, assignmentType);

        if (operator == null)
        {
            SemanticError error = SemanticError.undefinedAssignmentOperator(assignmentToken.getSpan(),
                                                                            assignmentToken.getSyntax(),
                                                                            symbolType, assignmentType);
            errorHandler.addError(error);
            return new AnnotatedLiteralExpression(null);
        }

        return new AnnotatedAssignmentExpression(name, operator, expression);
    }
}
