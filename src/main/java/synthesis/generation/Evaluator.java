package synthesis.generation;

import analysis.semantic.*;
import errors.ErrorHandler;
import errors.EvaluationError;
import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;
import identifiers.OperatorType;
import symbols.SymbolTable;

public final class Evaluator
{
    private final AnnotatedParseTree annotatedParseTree;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;
    private Object lastValue;

    public Evaluator(TypeChecker typeChecker, ErrorHandler errorHandler, SymbolTable symbolTable)
    {
        this.annotatedParseTree = typeChecker.getAnnotatedParseTree();
        this.errorHandler = errorHandler;
        this.symbolTable = symbolTable;
    }

    public Object getEvaluation()
    {
        return this.evaluate();
    }

    private Object evaluate()
    {
        try
        {
            this.evaluateStatement(this.annotatedParseTree.getAnnotatedStatement());
            return this.lastValue;
        } catch (Exception exception)
        {
            String message = EvaluationError.exceptionOccurred(exception);
            System.out.println(message);
            return null;
        }
    }

    private void evaluateStatement(AnnotatedStatement statement) throws Exception
    {
        switch (statement.getAnnotatedStatementType())
        {
            case ANNOTATED_BLOCK_STATEMENT:
                this.evaluateBlockStatement((AnnotatedBlockStatement) statement);
                break;
            case ANNOTATED_EXPRESSION_STATEMENT:
                this.evaluateExpressionStatement((AnnotatedExpressionStatement) statement);
                break;
            default:
                throw EvaluationError.unexpectedStatement(statement.getAnnotatedStatementType().toString());
        }
    }

    private void evaluateBlockStatement(AnnotatedBlockStatement annotatedBlockStatement) throws Exception
    {
        for (AnnotatedStatement statement : annotatedBlockStatement.getAnnotatedStatementList())
            this.evaluateStatement(statement);
    }

    private void evaluateExpressionStatement(AnnotatedExpressionStatement annotatedExpressionStatement) throws Exception
    {
        this.lastValue = this.evaluateExpression(annotatedExpressionStatement.getAnnotatedExpression());
    }

    private Object evaluateExpression(AnnotatedExpression expression) throws Exception
    {
        AnnotatedExpressionType type = expression.getAnnotatedExpressionType();

        if (type == AnnotatedExpressionType.ANNOTATED_LITERAL_EXPRESSION)
            return this.evaluateNumberExpression((AnnotatedLiteralExpression) expression);

        if (type == AnnotatedExpressionType.ANNOTATED_UNARY_EXPRESSION)
            return this.evaluateUnaryExpression((AnnotatedUnaryExpression) expression);

        if (type == AnnotatedExpressionType.ANNOTATED_BINARY_EXPRESSION)
            return this.evaluateBinaryExpression((AnnotatedBinaryExpression) expression);

        if (type == AnnotatedExpressionType.ANNOTATED_IDENTIFIER_EXPRESSION)
            return this.evaluateVariableExpression((AnnotatedIdentifierExpression) expression);

        if (type == AnnotatedExpressionType.ANNOTATED_ASSIGNMENT_EXPRESSION)
            return this.evaluateAssignmentExpression((AnnotatedAssignmentExpression) expression);

        throw EvaluationError.unexpectedExpression(expression.getAnnotatedExpressionType().toString());
    }

    private Object evaluateNumberExpression(AnnotatedLiteralExpression expression)
    {
        return expression.getValue();
    }

    private Object evaluateUnaryExpression(AnnotatedUnaryExpression expression) throws Exception
    {
        Object operand = this.evaluateExpression(expression.getOperand());
        ObjectType type = expression.getObjectType();
        OperatorType operatorType = expression.getOperator().getOperatorType();

        if (type == ObjectType.INTEGER_OBJECT)
            return this.evaluateUnaryIntegerExpression(operand, operatorType);

        if (type == ObjectType.DOUBLE_OBJECT)
            return this.evaluateUnaryDoubleExpression(operand, operatorType);

        if (type == ObjectType.BOOLEAN_OBJECT)
            return this.evaluateUnaryBooleanExpression(operand, operatorType);

        throw EvaluationError.unexpectedUnaryObjectType(type.toString());
    }

    private Object evaluateUnaryIntegerExpression(Object operand, OperatorType operatorType) throws Exception
    {
        int operandInt = (int) operand;

        switch (operatorType)
        {
            case IDENTITY_OPERATOR:
                return operandInt;
            case NEGATION_OPERATOR:
                return -operandInt;
            case LOGIC_NEGATION_OPERATOR:
                return !(boolean) operand;
            default:
                throw EvaluationError.unexpectedUnaryOperator(operatorType.toString());
        }
    }

    private Object evaluateUnaryDoubleExpression(Object operand, OperatorType operatorType) throws Exception
    {
        double operandDouble = (double) operand;

        switch (operatorType)
        {
            case IDENTITY_OPERATOR:
                return operandDouble;
            case NEGATION_OPERATOR:
                return -operandDouble;
            case LOGIC_NEGATION_OPERATOR:
                return !(boolean) operand;
            default:
                throw EvaluationError.unexpectedUnaryOperator(operatorType.toString());
        }
    }

    private Object evaluateUnaryBooleanExpression(Object operand, OperatorType operatorType) throws Exception
    {
        boolean operandBoolean = (boolean) operand;

        switch (operatorType)
        {
            case LOGIC_NEGATION_OPERATOR:
                return !operandBoolean;
            default:
                throw EvaluationError.unexpectedUnaryOperator(operatorType.toString());
        }
    }

    private Object evaluateBinaryExpression(AnnotatedBinaryExpression expression) throws Exception
    {
        Object left = this.evaluateExpression(expression.getLeftTerm());
        Object right = this.evaluateExpression(expression.getRightTerm());
        ObjectType leftType = expression.getLeftTerm().getObjectType();
        ObjectType rightType = expression.getRightTerm().getObjectType();
        OperatorType operatorType = expression.getOperator().getOperatorType();

        if (leftType == ObjectType.INTEGER_OBJECT && rightType == ObjectType.INTEGER_OBJECT)
            return this.evaluateBinaryIntegerExpression(left, right, operatorType);

        if (leftType == ObjectType.DOUBLE_OBJECT && rightType == ObjectType.DOUBLE_OBJECT)
            return this.evaluateBinaryDoubleExpression(left, right, operatorType);

        if (leftType == ObjectType.BOOLEAN_OBJECT && rightType == ObjectType.BOOLEAN_OBJECT)
            return this.evaluateBinaryBooleanExpression(left, right, operatorType);

        if (leftType == ObjectType.STRING_OBJECT && rightType == ObjectType.STRING_OBJECT)
            return this.evaluateBinaryStringExpression(left, right, operatorType);

        throw EvaluationError.unexpectedBinaryObjectTypes(leftType.toString(), rightType.toString());
    }

    private Object evaluateBinaryIntegerExpression(Object left, Object right, OperatorType operatorType) throws Exception
    {
        int leftInt = (int) left;
        int rightInt = (int) right;

        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return leftInt + rightInt;
            case SUBTRACTION_OPERATOR:
                return leftInt - rightInt;
            case MULTIPLICATION_OPERATOR:
                return leftInt * rightInt;
            case DIVISION_OPERATOR:
                if (rightInt == 0) return 0;
                return leftInt / rightInt;
            case POWER_OPERATOR:
                return (int) Math.pow(leftInt, rightInt);
            case MODULO_OPERATOR:
                if (rightInt == 0) return 0;
                return leftInt % rightInt;
            case GREATER_OPERATOR:
                return leftInt > rightInt;
            case LESS_OPERATOR:
                return leftInt < rightInt;
            case GREATER_EQUALS_OPERATOR:
                return leftInt >= rightInt;
            case LESS_EQUALS_OPERATOR:
                return leftInt <= rightInt;
            case EQUALS_EQUALS_OPERATOR:
                return leftInt == rightInt;
            case NOT_EQUALS_OPERATOR:
                return leftInt != rightInt;
            default:
                throw EvaluationError.unexpectedBinaryOperator(operatorType.toString());
        }
    }

    private Object evaluateBinaryDoubleExpression(Object left, Object right, OperatorType operatorType) throws Exception
    {
        double leftDouble = (double) left;
        double rightDouble = (double) right;

        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return leftDouble + rightDouble;
            case SUBTRACTION_OPERATOR:
                return leftDouble - rightDouble;
            case MULTIPLICATION_OPERATOR:
                return leftDouble * rightDouble;
            case DIVISION_OPERATOR:
                if (rightDouble == 0.0) return 0.0;
                return leftDouble / rightDouble;
            case POWER_OPERATOR:
                return Math.pow(leftDouble, rightDouble);
            case MODULO_OPERATOR:
                if (rightDouble == 0.0) return 0.0;
                return leftDouble % rightDouble;
            case GREATER_OPERATOR:
                return leftDouble > rightDouble;
            case LESS_OPERATOR:
                return leftDouble < rightDouble;
            case GREATER_EQUALS_OPERATOR:
                return leftDouble >= rightDouble;
            case LESS_EQUALS_OPERATOR:
                return leftDouble <= rightDouble;
            case EQUALS_EQUALS_OPERATOR:
                return leftDouble == rightDouble;
            case NOT_EQUALS_OPERATOR:
                return leftDouble != rightDouble;
            default:
                throw EvaluationError.unexpectedBinaryOperator(operatorType.toString());
        }
    }

    private Object evaluateBinaryBooleanExpression(Object left, Object right, OperatorType operatorType) throws Exception
    {
        boolean leftBool = (boolean) left;
        boolean rightBool = (boolean) right;

        switch (operatorType)
        {
            case AND_OPERATOR:
                return leftBool && rightBool;
            case OR_OPERATOR:
                return leftBool || rightBool;
            case EQUALS_EQUALS_OPERATOR:
                return leftBool == rightBool;
            case NOT_EQUALS_OPERATOR:
                return leftBool != rightBool;
            default:
                throw EvaluationError.unexpectedBinaryOperator(operatorType.toString());
        }
    }

    private Object evaluateBinaryStringExpression(Object left, Object right, OperatorType operatorType) throws Exception
    {
        String leftString = left.toString();
        String rightString = right.toString();

        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return leftString + rightString;
            case EQUALS_EQUALS_OPERATOR:
                return leftString.equals(rightString);
            case NOT_EQUALS_OPERATOR:
                return !leftString.equals(rightString);
            default:
                throw EvaluationError.unexpectedBinaryOperator(operatorType.toString());
        }
    }

    private Object evaluateVariableExpression(AnnotatedIdentifierExpression expression) throws Exception
    {
        String name = expression.getName();
        if (this.symbolTable.containsSymbol(name))
            return this.symbolTable.getSymbol(name).getValue();
        throw EvaluationError.missingSymbol(name);
    }

    private Object evaluateAssignmentExpression(AnnotatedAssignmentExpression expression) throws Exception
    {
        Object value = this.evaluateExpression(expression.getExpression());
        if (expression.getObjectType() != ObjectType.NULL_OBJECT)
            this.symbolTable.addSymbol(expression.getName(), value, expression.getObjectType());
        return value;
    }
}
