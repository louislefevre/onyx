package analysis.semantic;

import analysis.syntax.*;
import errors.ErrorHandler;
import errors.SemanticError;
import identifiers.ObjectType;
import org.jetbrains.annotations.Nullable;
import symbols.SymbolTable;

import java.util.ArrayList;
import java.util.List;

public final class TypeChecker
{
    private final ParseTree parseTree;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;

    public TypeChecker(Parser parser, ErrorHandler errorHandler, SymbolTable symbolTable)
    {
        this.parseTree = parser.getParseTree();
        this.errorHandler = errorHandler;
        this.symbolTable = symbolTable;
    }

    public AnnotatedParseTree getAnnotatedParseTree()
    {
        return new AnnotatedParseTree(this.getAnnotatedStatement());
    }

    private AnnotatedStatement getAnnotatedStatement()
    {
        return this.annotateStatement(this.parseTree.getStatement());
    }

    private AnnotatedStatement annotateStatement(Statement statement)
    {
        switch (statement.getStatementType())
        {
            case BLOCK_STATEMENT:
                return this.annotateBlockStatement((BlockStatement) statement);
            case EXPRESSION_STATEMENT:
                return this.annotateExpressionStatement((ExpressionStatement) statement);
            default:
                return this.unknownStatement(statement);
        }
    }

    private AnnotatedStatement annotateBlockStatement(BlockStatement blockStatement)
    {
        List<AnnotatedStatement> statementList = new ArrayList<>();

        for (Statement statementSyntax : blockStatement.getStatements())
        {
            AnnotatedStatement statement = this.annotateStatement(statementSyntax);
            statementList.add(statement);
        }

        return new AnnotatedBlockStatement(statementList);
    }

    private AnnotatedStatement annotateExpressionStatement(ExpressionStatement statement)
    {
        AnnotatedExpression expression = this.annotateExpression(statement.getExpression());
        return new AnnotatedExpressionStatement(expression);
    }

    private AnnotatedExpression annotateExpression(Expression expression)
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
                return this.unknownExpression(expression);
        }
    }

    private AnnotatedExpression annotateParenthesizedExpression(ParenthesizedExpression expression)
    {
        return this.annotateExpression(expression.getExpression());
    }

    private AnnotatedExpression annotateLiteralExpression(LiteralExpression expression)
    {
        Object value = expression.getValue();
        return new AnnotatedLiteralExpression(value);
    }

    private AnnotatedExpression annotateUnaryExpression(UnaryExpression expression)
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

    private AnnotatedExpression annotateBinaryExpression(BinaryExpression expression)
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

        return new AnnotatedVariableExpression(name, type);
    }

    private AnnotatedExpression annotateAssignmentExpression(AssignmentExpression expression)
    {
        String name = expression.getIdentifierToken().getSyntax();
        AnnotatedExpression annotatedExpression = this.annotateExpression(expression.getExpression());
        return new AnnotatedAssignmentExpression(name, annotatedExpression);
    }

    @Nullable
    private AnnotatedExpression unknownExpression(Expression expression)
    {
        try
        {
            throw SemanticError.undefinedExpression(expression.getExpressionType().toString());
        } catch (Exception err)
        {
            System.out.println(err.getMessage());
        }
        return null;
    }

    @Nullable
    private AnnotatedStatement unknownStatement(Statement statement)
    {
        try
        {
            throw SemanticError.undefinedStatement(statement.getStatementType().toString());
        } catch (Exception err)
        {
            System.out.println(err.getMessage());
        }
        return null;
    }
}
