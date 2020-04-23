package synthesis.generation;

import analysis.semantic.*;
import errors.ErrorHandler;
import errors.EvaluateError;
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
            StackTraceElement stackTraceElement = exception.getStackTrace()[0];
            int lineNumber = stackTraceElement.getLineNumber();
            String className = stackTraceElement.getClassName();

            String location = String.format("Line %1s: Exception occurred at %2s", lineNumber, className);
            String message = exception.getMessage();

            System.out.println(location);
            System.out.println(message);
        }
        return null;
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

        throw EvaluateError.unexpectedExpression(expression.getExpressionType().toString());
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
                throw EvaluateError.unexpectedUnaryOperator(operatorType.toString());
        }
    }

    private Object evaluateBinaryExpression(AnnotatedBinaryExpression expression) throws Exception
    {
        Object left = this.evaluateExpression(expression.getLeftTerm());
        Object right = this.evaluateExpression(expression.getRightTerm());
        OperatorType operatorType = expression.getOperator().getOperatorType();
        Object result = null;

        if((left instanceof Integer && right instanceof Integer) ||
           (left instanceof Boolean && right instanceof Boolean))
            result =  this.evaluateBinaryPrimitiveExpression(left, right, operatorType);

        if(left instanceof String && right instanceof String)
            result =  this.evaluateBinaryStringExpression(left, right, operatorType);

        if(result != null)
            return result;

        throw EvaluateError.unexpectedBinaryOperator(operatorType.toString());
    }

    private Object evaluateBinaryPrimitiveExpression(Object left, Object right, OperatorType operatorType)
    {
        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return (int) left + (int) right;
            case SUBTRACTION_OPERATOR:
                return (int) left - (int) right;
            case MULTIPLICATION_OPERATOR:
                return (int) left * (int) right;
            case DIVISION_OPERATOR:
                if ((int) right == 0) return 0;
                return (int) left / (int) right;
            case POWER_OPERATOR:
                return (int) Math.pow((int) left, (int) right);
            case MODULO_OPERATOR:
                if ((int) right == 0) return 0;
                return (int) left % (int) right;
            case AND_OPERATOR:
                return (boolean) left && (boolean) right;
            case OR_OPERATOR:
                return (boolean) left || (boolean) right;
            case EQUALS_EQUALS_OPERATOR:
                return left == right;
            case NOT_EQUALS_OPERATOR:
                return left != right;
            case GREATER_OPERATOR:
                return (int) left > (int) right;
            case LESS_OPERATOR:
                return (int) left < (int) right;
            case GREATER_EQUALS_OPERATOR:
                return (int) left >= (int) right;
            case LESS_EQUALS_OPERATOR:
                return (int) left <= (int) right;
            default:
                return null;
        }
    }

    private Object evaluateBinaryStringExpression(Object left, Object right, OperatorType operatorType)
    {
        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return left.toString() + right.toString();
            case EQUALS_EQUALS_OPERATOR:
                return left.toString().equals(right.toString());
            default:
                return null;
        }
    }

    private Object evaluateVariableExpression(AnnotatedVariableExpression expression) throws Exception
    {
        String name = expression.getName();
        if (this.symbolTable.containsSymbol(name))
            return this.symbolTable.getSymbol(expression.getName()).getValue();
        throw EvaluateError.missingSymbol(name);
    }

    private Object evaluateAssignmentExpression(AnnotatedAssignmentExpression expression) throws Exception
    {
        Object value = this.evaluateExpression(expression.getExpression());
        this.symbolTable.addSymbol(expression.getName(), value, expression.getObjectType());
        return value;
    }
}
