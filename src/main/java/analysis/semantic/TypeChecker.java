package analysis.semantic;

import analysis.syntax.*;
import errors.ErrorHandler;
import errors.SemanticError;
import identifiers.ObjectType;
import symbols.SymbolTable;

import java.util.ArrayList;
import java.util.List;

public final class TypeChecker
{
    private final ParseTree parseTree;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;

    public TypeChecker(Parser parser)
    {
        this.parseTree = parser.getParseTree();
        this.errorHandler = parser.getErrorHandler();
        this.symbolTable = parser.getSymbolTable();
    }

    public ErrorHandler getErrorHandler()
    {
        return this.errorHandler;
    }

    public SymbolTable getSymbolTable()
    {
        return symbolTable;
    }

    public AnnotatedParseTree getAnnotatedParseTree()
    {
        return new AnnotatedParseTree(this.getAnnotatedStatement());
    }

    private AnnotatedStatement getAnnotatedStatement()
    {
        try
        {
            return this.annotateStatement(this.parseTree.getStatement());
        } catch (Exception exception)
        {
            String message = SemanticError.exceptionOccurred(exception);
            System.out.println(message);
            return null;
        }
    }

    private AnnotatedStatement annotateStatement(Statement statement) throws Exception
    {
        switch (statement.getStatementType())
        {
            case BLOCK_STATEMENT:
                return this.annotateBlockStatement((BlockStatement) statement);
            case EXPRESSION_STATEMENT:
                return this.annotateExpressionStatement((ExpressionStatement) statement);
            default:
                throw new Exception(SemanticError.undefinedStatement(statement.getStatementType().toString()));
        }
    }

    private AnnotatedStatement annotateBlockStatement(BlockStatement blockStatement) throws Exception
    {
        List<AnnotatedStatement> statements = new ArrayList<>();

        for (Statement statement : blockStatement.getStatements())
        {
            AnnotatedStatement annotatedStatement = this.annotateStatement(statement);
            statements.add(annotatedStatement);
        }

        return new AnnotatedBlockStatement(statements);
    }

    private AnnotatedStatement annotateExpressionStatement(ExpressionStatement expressionStatement) throws Exception
    {
        Expression expression = expressionStatement.getExpression();
        AnnotatedExpression annotatedExpression = this.annotateExpression(expression);
        return new AnnotatedExpressionStatement(annotatedExpression);
    }

    private AnnotatedExpression annotateExpression(Expression expression) throws Exception
    {
        switch (expression.getExpressionType())
        {
            case LITERAL_EXPRESSION:
                return this.annotateLiteralExpression((LiteralExpression) expression);
            case UNARY_EXPRESSION:
                return this.annotateUnaryExpression((UnaryExpression) expression);
            case BINARY_EXPRESSION:
                return this.annotateBinaryExpression((BinaryExpression) expression);
            case IDENTIFIER_EXPRESSION:
                return this.annotateIdentifierExpression((IdentifierExpression) expression);
            case ASSIGNMENT_EXPRESSION:
                return this.annotateAssignmentExpression((AssignmentExpression) expression);
            case PARENTHESIZED_EXPRESSION:
                return this.annotateParenthesizedExpression((ParenthesizedExpression) expression);
            default:
                throw new Exception(SemanticError.undefinedExpression(expression.getExpressionType().toString()));
        }
    }

    private AnnotatedExpression annotateParenthesizedExpression(ParenthesizedExpression parenthesizedExpression) throws Exception
    {
        Expression expression = parenthesizedExpression.getExpression();
        return this.annotateExpression(expression);
    }

    private AnnotatedExpression annotateLiteralExpression(LiteralExpression literalExpression)
    {
        Object value = literalExpression.getValue();
        return new AnnotatedLiteralExpression(value);
    }

    private AnnotatedExpression annotateUnaryExpression(UnaryExpression unaryExpression) throws Exception
    {
        AnnotatedExpression annotatedOperand = this.annotateExpression(unaryExpression.getOperand());
        AnnotatedUnaryOperator annotatedOperator =
                TypeBinder.bindUnaryOperators(unaryExpression.getOperatorToken().getTokenType(),
                                              annotatedOperand.getObjectType());

        if (annotatedOperator == null)
        {
            this.errorHandler.addError(SemanticError.undefinedUnaryOperator(unaryExpression.getOperatorToken().getSpan(),
                                                                            unaryExpression.getOperatorToken().getSyntax(),
                                                                            annotatedOperand.getObjectType()));
            return new AnnotatedLiteralExpression(null);
        }

        return new AnnotatedUnaryExpression(annotatedOperator, annotatedOperand);
    }

    private AnnotatedExpression annotateBinaryExpression(BinaryExpression binaryExpression) throws Exception
    {
        AnnotatedExpression annotatedLeftOperand = this.annotateExpression(binaryExpression.getLeftOperand());
        AnnotatedExpression annotatedRightOperand = this.annotateExpression(binaryExpression.getRightOperand());
        AnnotatedBinaryOperator annotatedOperator =
                TypeBinder.bindBinaryOperators(binaryExpression.getOperatorToken().getTokenType(),
                                               annotatedLeftOperand.getObjectType(),
                                               annotatedRightOperand.getObjectType());

        if (annotatedOperator == null)
        {
            this.errorHandler.addError(SemanticError.undefinedBinaryOperator(binaryExpression.getOperatorToken().getSpan(),
                                                                             binaryExpression.getOperatorToken().getSyntax(),
                                                                             annotatedLeftOperand.getObjectType(),
                                                                             annotatedRightOperand.getObjectType()));
            return new AnnotatedLiteralExpression(null);
        }

        return new AnnotatedBinaryExpression(annotatedLeftOperand, annotatedOperator, annotatedRightOperand);
    }

    private AnnotatedExpression annotateIdentifierExpression(IdentifierExpression identifierExpression)
    {
        String name = identifierExpression.getIdentifierToken().getSyntax();

        if (!this.symbolTable.containsSymbol(name))
        {
            this.errorHandler.addError(SemanticError.undefinedIdentifier(identifierExpression.getIdentifierToken().getSpan(),
                                                                         name));
            return new AnnotatedLiteralExpression(null);
        }
        ObjectType type = this.symbolTable.getSymbol(name).getType();

        return new AnnotatedIdentifierExpression(name, type);
    }

    private AnnotatedExpression annotateAssignmentExpression(AssignmentExpression assignmentExpression) throws Exception
    {
        String name = assignmentExpression.getIdentifierToken().getSyntax();
        AnnotatedExpression expression = this.annotateExpression(assignmentExpression.getExpression());
        return new AnnotatedAssignmentExpression(name, expression);
    }
}
