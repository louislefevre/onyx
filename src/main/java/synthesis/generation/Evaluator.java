package synthesis.generation;

import analysis.semantic.*;
import errors.ErrorHandler;
import identifiers.OperatorType;

import java.util.HashMap;

public final class Evaluator
{
    private final AnnotatedParseTree annotatedParseTree;
    private final ErrorHandler errorHandler;
    private final HashMap<String, Object> variables;

    public Evaluator(TypeChecker typeChecker, ErrorHandler errorHandler)
    {
        this.annotatedParseTree = typeChecker.getAnnotatedParseTree();
        this.errorHandler = errorHandler;
        this.variables = typeChecker.getVariables();
    }

    public Object getEvaluation()
    {
        return this.evaluate();
    }

    private Object evaluate()
    {
        try
        {
            return this.evaluateExpression(this.annotatedParseTree.getExpression());
        }
        catch(Exception exception)
        {
            System.out.println(exception.getMessage());
        }
        return null;
    }

    private Object evaluateExpression(AnnotatedExpression node) throws Exception
    {
        if(node instanceof AnnotatedLiteralExpression)
            return this.evaluateNumberExpression((AnnotatedLiteralExpression)node);

        if(node instanceof AnnotatedUnaryExpression)
            return this.evaluateUnaryExpression((AnnotatedUnaryExpression)node);

        if(node instanceof AnnotatedBinaryExpression)
            return this.evaluateBinaryExpression((AnnotatedBinaryExpression)node);

        if(node instanceof AnnotatedVariableExpression)
            return this.evaluateVariableExpression((AnnotatedVariableExpression)node);

        if(node instanceof AnnotatedAssignmentExpression)
            return this.evaluateAssignmentExpression((AnnotatedAssignmentExpression)node);

        throw new Exception(String.format("EXCEPTION: Unexpected node '%s'.", node.getObjectType()));
    }

    private Object evaluateNumberExpression(AnnotatedLiteralExpression node)
    {
        return node.getValue();
    }

    private Object evaluateUnaryExpression(AnnotatedUnaryExpression node) throws Exception
    {
        Object operand = this.evaluateExpression(node.getOperand());
        OperatorType operatorType = node.getOperator().getOperatorType();

        switch(operatorType)
        {
            case IDENTITY_OPERATOR:
                return operand;
            case NEGATION_OPERATOR:
                return -(int)operand;
            case LOGIC_NEGATION_OPERATOR:
                return !(boolean)operand;
            default:
                throw new Exception(String.format("EXCEPTION: Unexpected unary operator '%s'.", operatorType));
        }
    }

    private Object evaluateBinaryExpression(AnnotatedBinaryExpression node) throws Exception
    {
        Object left = this.evaluateExpression(node.getLeftTerm());
        Object right = this.evaluateExpression(node.getRightTerm());
        OperatorType tokenKind = node.getOperator().getOperatorType();

        switch(tokenKind)
        {
            case ADDITION_OPERATOR:
                return (int)left + (int)right;
            case SUBTRACTION_OPERATOR:
                return (int)left - (int)right;
            case MULTIPLICATION_OPERATOR:
                return (int)left * (int)right;
            case DIVISION_OPERATOR:
                return (int)left / (int)right;
            case POWER_OPERATOR:
                return (int)Math.pow((int)left, (int)right);
            case MODULO_OPERATOR:
                return (int)left % (int)right;
            case AND_OPERATOR:
                return (boolean)left && (boolean)right;
            case OR_OPERATOR:
                return (boolean)left || (boolean)right;
            case EQUALS_EQUALS_OPERATOR:
                return left == right;
            case NOT_EQUALS_OPERATOR:
                return left != right;
            case GREATER_OPERATOR:
                return (int)left > (int)right;
            case LESS_OPERATOR:
                return (int)left < (int)right;
            case GREATER_EQUALS_OPERATOR:
                return (int)left >= (int)right;
            case LESS_EQUALS_OPERATOR:
                return (int)left <= (int)right;
            default:
                throw new Exception(String.format("EXCEPTION: Unexpected binary operator '%s'.", tokenKind));
        }
    }

    private Object evaluateVariableExpression(AnnotatedVariableExpression node)
    {
        return this.variables.get(node.getName());
    }

    private Object evaluateAssignmentExpression(AnnotatedAssignmentExpression node) throws Exception
    {
        Object value = this.evaluateExpression(node.getExpression());
        this.variables.put(node.getName(), value);
        return value;
    }
}
