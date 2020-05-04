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

    public TypeChecker(Parser parser)
    {
        this.parseTree = parser.getParseTree();
        this.errorHandler = parser.getErrorHandler();
        this.symbolTable = parser.getSymbolTable();
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
            case CONDITIONAL_STATEMENT:
                return this.annotateConditionalStatement((ConditionalStatement) statement);
            case LOOP_STATEMENT:
                return this.annotateLoopStatement((LoopStatement) statement);
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

    private AnnotatedStatement annotateConditionalStatement(ConditionalStatement conditionalStatement) throws Exception
    {
        AnnotatedExpression annotatedCondition = this.annotateExpression(conditionalStatement.getConditionExpression());

        if (annotatedCondition.getObjectType() != ObjectType.BOOLEAN_OBJECT)
            this.errorHandler.addError(SemanticError.invalidConditionalTypes(conditionalStatement.getIfToken().getSpan(),
                                                                             annotatedCondition.getObjectType(),
                                                                             ObjectType.BOOLEAN_OBJECT));

        AnnotatedStatement annotatedThenStatement = this.annotateStatement(conditionalStatement.getThenStatement());

        AnnotatedStatement annotatedElseClause;
        if (conditionalStatement.includesElseStatement())
            annotatedElseClause = this.annotateStatement(conditionalStatement.getElseStatement().getStatement());
        else
            annotatedElseClause = null;

        return new AnnotatedConditionalStatement(annotatedCondition, annotatedThenStatement, annotatedElseClause);
    }

    private AnnotatedStatement annotateLoopStatement(LoopStatement loopStatement) throws Exception
    {
        String name = loopStatement.getIdentifierToken().getSyntax();
        AnnotatedExpression lowerBound = this.annotateExpression(loopStatement.getLowerBound());
        AnnotatedExpression upperBound = this.annotateExpression(loopStatement.getUpperBound());

        if(lowerBound.getObjectType() != ObjectType.INTEGER_OBJECT)
            this.errorHandler.addError(SemanticError.invalidConditionalTypes(loopStatement.getLoopToken().getSpan(),
                                                                             lowerBound.getObjectType(),
                                                                             ObjectType.INTEGER_OBJECT));
        if(upperBound.getObjectType() != ObjectType.INTEGER_OBJECT)
            this.errorHandler.addError(SemanticError.invalidConditionalTypes(loopStatement.getLoopToken().getSpan(),
                                                                             upperBound.getObjectType(),
                                                                             ObjectType.INTEGER_OBJECT));

        Symbol symbol = new Symbol(name, null, ObjectType.INTEGER_OBJECT);
        this.symbolTable.addSymbol(symbol);

        AnnotatedStatement body = this.annotateStatement(loopStatement.getBody());

        return new AnnotatedLoopStatement(symbol, lowerBound, upperBound, body);
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
        Token assignmentToken = assignmentExpression.getAssignmentToken();
        TokenType assignmentTokenType = assignmentToken.getTokenType();
        AnnotatedExpression expression = this.annotateExpression(assignmentExpression.getExpression());
        ObjectType assignmentType = expression.getObjectType();

        ObjectType symbolType = this.symbolTable.containsSymbol(name) ?
                                this.symbolTable.getSymbol(name).getType() : ObjectType.NULL_OBJECT;

        AnnotatedAssignmentOperator annotatedOperator =
                TypeBinder.bindAssignmentOperators(assignmentTokenType, symbolType, assignmentType);

        if (annotatedOperator == null)
        {
            this.errorHandler.addError(SemanticError.undefinedAssignmentOperator(assignmentToken.getSpan(), assignmentToken.getSyntax(), symbolType, assignmentType));
            return new AnnotatedLiteralExpression(null);
        }

        return new AnnotatedAssignmentExpression(name, annotatedOperator, expression);
    }
}
