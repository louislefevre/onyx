package analysis.semantic;

import analysis.syntax.*;
import errors.ErrorHandler;
import errors.SemanticError;
import identifiers.ObjectType;
import symbols.Symbol;
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
            case DECLARATION_STATEMENT:
                return this.annotatedDeclarationStatement((DeclarationStatement) statement);
            default:
                throw SemanticError.undefinedStatement(statement.getStatementType().toString());
        }
    }

    private AnnotatedStatement annotateBlockStatement(BlockStatement blockStatement) throws Exception
    {
        List<AnnotatedStatement> statementList = new ArrayList<>();

        for (Statement statementSyntax : blockStatement.getStatements())
        {
            AnnotatedStatement statement = this.annotateStatement(statementSyntax);
            statementList.add(statement);
        }

        return new AnnotatedBlockStatement(statementList);
    }

    private AnnotatedStatement annotateExpressionStatement(ExpressionStatement statement) throws Exception
    {
        AnnotatedExpression expression = this.annotateExpression(statement.getExpression());
        return new AnnotatedExpressionStatement(expression);
    }

    private AnnotatedStatement annotatedDeclarationStatement(DeclarationStatement statement) throws Exception
    {
        String name = statement.getIdentifierToken().getSyntax();
        AnnotatedExpression initializerExpression = this.annotateExpression(statement.getInitializerExpression());
        Symbol symbol = new Symbol(name, null, initializerExpression.getObjectType());

        if (this.symbolTable.containsSymbol(name))
            this.errorHandler.addError(SemanticError.declaredVariable(statement.getIdentifierToken().getSpan(), name));

        return new AnnotatedDeclarationStatement(symbol, initializerExpression);
    }

    private AnnotatedExpression annotateExpression(Expression expression) throws Exception
    {
        switch (expression.getExpressionType())
        {
            case PARENTHESIZED_EXPRESSION:
                return this.annotateParenthesizedExpression((ParenthesizedExpression) expression);
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
            default:
                throw SemanticError.undefinedExpression(expression.getExpressionType().toString());
        }
    }

    private AnnotatedExpression annotateParenthesizedExpression(ParenthesizedExpression expression) throws Exception
    {
        return this.annotateExpression(expression.getExpression());
    }

    private AnnotatedExpression annotateLiteralExpression(LiteralExpression expression)
    {
        Object value = expression.getValue();
        return new AnnotatedLiteralExpression(value);
    }

    private AnnotatedExpression annotateUnaryExpression(UnaryExpression expression) throws Exception
    {
        AnnotatedExpression annotatedOperand = this.annotateExpression(expression.getOperand());
        AnnotatedUnaryOperator annotatedOperator =
                TypeBinder.bindUnaryOperators(expression.getOperatorToken().getTokenType(),
                                              annotatedOperand.getObjectType());

        if (annotatedOperator == null)
        {
            this.errorHandler.addError(SemanticError.undefinedUnaryOperator(expression.getOperatorToken().getSpan(),
                                                                            expression.getOperatorToken().getSyntax(),
                                                                            annotatedOperand.getObjectType()));
            return new AnnotatedLiteralExpression(null);
        }

        return new AnnotatedUnaryExpression(annotatedOperator, annotatedOperand);
    }

    private AnnotatedExpression annotateBinaryExpression(BinaryExpression expression) throws Exception
    {
        AnnotatedExpression annotatedLeft = this.annotateExpression(expression.getLeftTerm());
        AnnotatedExpression annotatedRight = this.annotateExpression(expression.getRightTerm());
        AnnotatedBinaryOperator annotatedOperator =
                TypeBinder.bindBinaryOperators(expression.getOperatorToken().getTokenType(),
                                               annotatedLeft.getObjectType(),
                                               annotatedRight.getObjectType());

        if (annotatedOperator == null)
        {
            this.errorHandler.addError(SemanticError.undefinedBinaryOperator(expression.getOperatorToken().getSpan(),
                                                                             expression.getOperatorToken().getSyntax(),
                                                                             annotatedLeft.getObjectType(),
                                                                             annotatedRight.getObjectType()));
            return new AnnotatedLiteralExpression(null);
        }

        return new AnnotatedBinaryExpression(annotatedLeft, annotatedOperator, annotatedRight);
    }

    private AnnotatedExpression annotateIdentifierExpression(IdentifierExpression expression)
    {
        String name = expression.getIdentifierToken().getSyntax();

        if (!this.symbolTable.containsSymbol(name))
        {
            this.errorHandler.addError(SemanticError.undefinedIdentifier(expression.getIdentifierToken().getSpan(),
                                                                         name));
            return new AnnotatedLiteralExpression(null);
        }
        ObjectType type = this.symbolTable.getSymbol(name).getType();

        return new AnnotatedIdentifierExpression(name, type);
    }

    private AnnotatedExpression annotateAssignmentExpression(AssignmentExpression expression) throws Exception
    {
        String name = expression.getIdentifierToken().getSyntax();
        AnnotatedExpression annotatedExpression = this.annotateExpression(expression.getExpression());

        if (!this.symbolTable.containsSymbol(name))
        {
            this.errorHandler.addError(SemanticError.undeclaredVariable(expression.getIdentifierToken().getSpan(),
                                                                        name));
            return annotatedExpression;
        }

        return new AnnotatedAssignmentExpression(name, annotatedExpression);
    }
}
