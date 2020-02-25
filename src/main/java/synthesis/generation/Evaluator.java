package synthesis.generation;

import analysis.semantic.*;
import errors.ErrorHandler;
import identifiers.ObjectType;
import identifiers.OperatorType;

public final class Evaluator
{
    private final AnnotatedParseTree annotatedParseTree;

    public Evaluator(TypeChecker typeChecker)
    {
        this.annotatedParseTree = typeChecker.getAnnotatedParseTree();
    }

    public Object evaluate()
    {
        return this.evaluateErrors(this.annotatedParseTree.getExpression());
    }

    private Object evaluateErrors(AnnotatedExpression expression)
    {
        if(!ErrorHandler.errorsPresent())
        {
            try
            {
                return evaluateExpression(expression);
            }
            catch(Exception error)
            {
                System.out.println(error.getMessage());
            }
        }
        ErrorHandler.printErrors();
        ErrorHandler.resetErrors();
        return ObjectType.NULL_OBJECT;
    }

    private Object evaluateExpression(AnnotatedExpression node) throws Exception
    {
        if(node instanceof AnnotatedLiteralExpression)
            return this.evaluateNumberExpression(node);

        if(node instanceof AnnotatedUnaryExpression)
            return this.evaluateUnaryExpression(node);

        if(node instanceof AnnotatedBinaryExpression)
            return this.evaluateBinaryExpression(node);

        throw new Exception(String.format("Unexpected node '%s'", node.getObjectType()));
    }

    private Object evaluateNumberExpression(AnnotatedExpression node)
    {
        return ((AnnotatedLiteralExpression) node).getValue();
    }

    private Object evaluateUnaryExpression(AnnotatedExpression node) throws Exception
    {
        Object operand = this.evaluateExpression(((AnnotatedUnaryExpression) node).getOperand());
        OperatorType operatorType = ((AnnotatedUnaryExpression) node).getOperator().getOperatorType();

        switch(operatorType)
        {
            case IDENTITY_OPERATOR:
                return operand;
            case NEGATION_OPERATOR:
                return -(int)operand;
            case LOGIC_NEGATION_OPERATOR:
                return !(boolean)operand;
            default:
                throw new Exception(String.format("Unexpected unary operator '%s'", operatorType));
        }
    }

    private Object evaluateBinaryExpression(AnnotatedExpression node) throws Exception
    {
        Object left = this.evaluateExpression(((AnnotatedBinaryExpression) node).getLeftTerm());
        Object right = this.evaluateExpression(((AnnotatedBinaryExpression) node).getRightTerm());
        OperatorType tokenKind = ((AnnotatedBinaryExpression) node).getOperator().getOperatorType();

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
                throw new Exception(String.format("Unexpected binary operator '%s'", tokenKind));
        }
    }
}
