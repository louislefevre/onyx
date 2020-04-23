package synthesis.generation;

import analysis.semantic.*;
import errors.ErrorHandler;
import errors.EvaluationError;
import identifiers.OperatorType;
import org.jetbrains.annotations.Nullable;
import symbols.SymbolTable;

public final class Evaluator
{
    private final AnnotatedParseTree annotatedParseTree;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;

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

    @Nullable
    private Object evaluate()
    {
        try
        {
            return this.evaluateExpression(this.annotatedParseTree.getExpression());
        } catch (Exception exception)
        {
            return EvaluationError.exceptionOccurred(exception);
        }
    }

    private Object evaluateExpression(AnnotatedExpression expression) throws Exception
    {
        if (expression instanceof AnnotatedLiteralExpression)
            return this.evaluateNumberExpression((AnnotatedLiteralExpression) expression);

        if (expression instanceof AnnotatedUnaryExpression)
            return this.evaluateUnaryExpression((AnnotatedUnaryExpression) expression);

        if (expression instanceof AnnotatedBinaryExpression)
            return this.evaluateBinaryExpression((AnnotatedBinaryExpression) expression);

        if (expression instanceof AnnotatedVariableExpression)
            return this.evaluateVariableExpression((AnnotatedVariableExpression) expression);

        if (expression instanceof AnnotatedAssignmentExpression)
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
        OperatorType operatorType = expression.getOperator().getOperatorType();

        switch (operatorType)
        {
            case IDENTITY_OPERATOR:
                return operand;
            case NEGATION_OPERATOR:
                return -(int) operand;
            case LOGIC_NEGATION_OPERATOR:
                return !(boolean) operand;
            default:
                throw EvaluationError.unexpectedUnaryOperator(operatorType.toString());
        }
    }

    private Object evaluateBinaryExpression(AnnotatedBinaryExpression expression) throws Exception
    {
        Object left = this.evaluateExpression(expression.getLeftTerm());
        Object right = this.evaluateExpression(expression.getRightTerm());
        OperatorType operatorType = expression.getOperator().getOperatorType();
        Object result = null;

        if (left instanceof Integer && right instanceof Integer)
            result = this.evaluateBinaryIntegerExpression(left, right, operatorType);

        if (left instanceof Boolean && right instanceof Boolean)
            result = this.evaluateBinaryBooleanExpression(left, right, operatorType);

        if (left instanceof String && right instanceof String)
            result = this.evaluateBinaryStringExpression(left, right, operatorType);

        if (result != null)
            return result;

        throw EvaluationError.unexpectedBinaryOperator(operatorType.toString());
    }

    private Object evaluateBinaryIntegerExpression(Object left, Object right, OperatorType operatorType)
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
                return null;
        }
    }

    private Object evaluateBinaryBooleanExpression(Object left, Object right, OperatorType operatorType)
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
                return null;
        }
    }

    private Object evaluateBinaryStringExpression(Object left, Object right, OperatorType operatorType)
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
                return null;
        }
    }

    private Object evaluateVariableExpression(AnnotatedVariableExpression expression) throws Exception
    {
        String name = expression.getName();
        if (this.symbolTable.containsSymbol(name))
            return this.symbolTable.getSymbol(name).getValue();
        throw EvaluationError.missingSymbol(name);
    }

    private Object evaluateAssignmentExpression(AnnotatedAssignmentExpression expression) throws Exception
    {
        Object value = this.evaluateExpression(expression.getExpression());
        this.symbolTable.addSymbol(expression.getName(), value, expression.getObjectType());
        return value;
    }
}
