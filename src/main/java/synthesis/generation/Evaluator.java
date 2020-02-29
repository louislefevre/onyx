package synthesis.generation;

import analysis.semantic.*;
import errors.Error;
import errors.EvaluateError;
import identifiers.OperatorType;

import java.util.List;

public final class Evaluator
{
    private final AnnotatedParseTree annotatedParseTree;
    private final List<Error> errorLog;

    public Evaluator(TypeChecker typeChecker)
    {
        this.annotatedParseTree = typeChecker.getAnnotatedParseTree();
        this.errorLog = typeChecker.getErrorLog();
    }

    public Object evaluate()
    {
        return this.evaluateExpression(this.annotatedParseTree.getExpression());
    }

    public List<Error> getErrorLog()
    {
        return this.errorLog;
    }

    private Object evaluateExpression(AnnotatedExpression node)
    {
        if(node instanceof AnnotatedLiteralExpression)
            return this.evaluateNumberExpression(node);

        if(node instanceof AnnotatedUnaryExpression)
            return this.evaluateUnaryExpression(node);

        if(node instanceof AnnotatedBinaryExpression)
            return this.evaluateBinaryExpression(node);

        this.errorLog.add(new EvaluateError(String.format("Unexpected node '%s'", node.getObjectType())));
        return null;
    }

    private Object evaluateNumberExpression(AnnotatedExpression node)
    {
        return ((AnnotatedLiteralExpression) node).getValue();
    }

    private Object evaluateUnaryExpression(AnnotatedExpression node)
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
                this.errorLog.add(new EvaluateError(String.format("Unexpected unary operator '%s'", operatorType)));
                return null;
        }
    }

    private Object evaluateBinaryExpression(AnnotatedExpression node)
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
                this.errorLog.add(new EvaluateError(String.format("Unexpected binary operator '%s'", tokenKind)));
                return null;
        }
    }
}
