package compilation.analysis.semantic;

import compilation.analysis.lexical.Token;
import compilation.analysis.syntax.*;
import errors.ErrorHandler;
import errors.SemanticError;
import exceptions.Exception;
import exceptions.SemanticException;
import source.SourceSpan;
import symbols.Symbol;
import symbols.SymbolTable;
import types.ObjectType;
import types.TokenType;

import java.util.ArrayList;
import java.util.List;

import static types.ObjectType.BOOLEAN_OBJECT;
import static types.ObjectType.DOUBLE_OBJECT;
import static types.ObjectType.INTEGER_OBJECT;
import static types.ObjectType.NULL_OBJECT;

public final class TypeChecker
{
    private final Parser parser;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;

    public TypeChecker(Parser parser, ErrorHandler errorHandler, SymbolTable symbolTable)
    {
        this.parser = parser;
        this.errorHandler = errorHandler;
        this.symbolTable = symbolTable;
    }

    public AnnotatedParseTree getAnnotatedParseTree() throws Exception
    {
        ParseTree parseTree = parser.getParseTree();
        Statement statement = parseTree.getStatement();
        AnnotatedStatement annotatedStatement = annotateStatement(statement);
        return new AnnotatedParseTree(annotatedStatement);
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
                String errorMessage = SemanticException.undefinedStatement(statement.getStatementType().toString());
                throw new SemanticException(errorMessage);
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
        Expression expression = conditionalStatement.getCondition();
        AnnotatedExpression condition = annotateExpression(expression);
        validateExpressionType(expression.getSpan(), condition.getObjectType(), BOOLEAN_OBJECT);

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
        AnnotatedExpression lowerBound = annotateExpression(loopStatement.getLowerBound());
        AnnotatedExpression upperBound = annotateExpression(loopStatement.getUpperBound());
        ObjectType lowerType = lowerBound.getObjectType();
        ObjectType upperType = upperBound.getObjectType();

        // Both need to be int or double, both need to match
        validateExpressionType(loopStatement.getLowerBound().getSpan(), lowerType, INTEGER_OBJECT, DOUBLE_OBJECT);
        validateExpressionType(loopStatement.getUpperBound().getSpan(), upperType, INTEGER_OBJECT, DOUBLE_OBJECT);
        validateExpressionType(loopStatement.getUpperBound().getSpan(), upperType, lowerType);

        AnnotatedStatement body = annotateStatement(loopStatement.getBody());

        return new AnnotatedLoopStatement(lowerBound, upperBound, body);
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
                String errorMessage = SemanticException.undefinedExpression(expression.getExpressionType().toString());
                throw new SemanticException(errorMessage);
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
            errorHandler.add(error);
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
            errorHandler.add(SemanticError.undefinedBinaryOperator(binaryExpression.getOperatorToken().getSpan(),
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

        if (!symbolTable.contains(name))
        {
            SemanticError error = SemanticError.undefinedIdentifier(identifierExpression.getIdentifierToken().getSpan(), name);
            errorHandler.add(error);
            return new AnnotatedLiteralExpression(null);
        }
        ObjectType type = symbolTable.get(name).getType();

        return new AnnotatedIdentifierExpression(name, type);
    }

    private AnnotatedExpression annotateAssignmentExpression(AssignmentExpression assignmentExpression) throws Exception
    {
        String name = assignmentExpression.getIdentifierExpression().getIdentifierToken().getSyntax();
        AnnotatedExpression expression = annotateExpression(assignmentExpression.getExpression());
        Token assignmentToken = assignmentExpression.getAssignmentToken();
        TokenType assignmentTokenType = assignmentToken.getType();
        ObjectType symbolType = symbolTable.contains(name) ? symbolTable.get(name).getType() : NULL_OBJECT;
        ObjectType assignmentType = expression.getObjectType();
        AnnotatedAssignmentOperator operator = TypeBinder.bindAssignmentOperators(assignmentTokenType, symbolType, assignmentType);

        if(!symbolTable.contains(name))
        {
            Symbol symbol = new Symbol(name, assignmentType);
            symbolTable.add(symbol);
        }

        if (operator == null)
        {
            SemanticError error = SemanticError.undefinedAssignmentOperator(assignmentToken.getSpan(),
                                                                            assignmentToken.getSyntax(),
                                                                            symbolType, assignmentType);
            errorHandler.add(error);
            return new AnnotatedLiteralExpression(null);
        }

        AnnotatedIdentifierExpression identifier = new AnnotatedIdentifierExpression(name, symbolType);

        return new AnnotatedAssignmentExpression(identifier, operator, expression);
    }

    private void validateExpressionType(SourceSpan span, ObjectType actualType, ObjectType... targetTypes)
    {
        for (ObjectType type : targetTypes)
            if (actualType == type)
                return;

        SemanticError error = SemanticError.invalidExpressionTypes(span, actualType, targetTypes);
        errorHandler.add(error);
    }
}
