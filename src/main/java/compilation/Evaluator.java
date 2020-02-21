package compilation;

import analysis.semantic.*;
import errors.ErrorHandler;
import symbols.OperatorType;

public final class Evaluator
{
    private final BoundExpression parseTree;

    public Evaluator(Binder binder)
    {
        this.parseTree = binder.getParseTree();
    }

    public Object evaluate()
    {
        if(!ErrorHandler.errorsPresent())
        {
            try
            {
                return evaluateExpression(this.parseTree);
            }
            catch(Exception error)
            {
                System.out.println(error.getMessage());
            }
        }
        return null;
    }

    private Object evaluateExpression(BoundExpression node) throws Exception
    {
        if(node instanceof BoundLiteralExpression)
            return this.evaluateNumberExpression(node);

        if(node instanceof BoundUnaryExpression)
            return this.evaluateUnaryExpression(node);

        if(node instanceof BoundBinaryExpression)
            return this.evaluateBinaryExpression(node);

        throw new Exception(String.format("Unexpected node '%s'", node.getObjectType()));
    }

    private Object evaluateNumberExpression(BoundExpression node)
    {
        return ((BoundLiteralExpression) node).getValue();
    }

    private Object evaluateUnaryExpression(BoundExpression node) throws Exception
    {
        Object operand = this.evaluateExpression(((BoundUnaryExpression) node).getOperand());
        OperatorType operatorType = ((BoundUnaryExpression) node).getOperator().getOperatorType();

        switch(operatorType)
        {
            case IdentityOperator:
                return operand;
            case NegationOperator:
                return -(int)operand;
            case LogicNegationOperator:
                return !(boolean)operand;
            default:
                throw new Exception(String.format("Unexpected unary operator '%s'", operatorType));
        }
    }

    private Object evaluateBinaryExpression(BoundExpression node) throws Exception
    {
        Object left = this.evaluateExpression(((BoundBinaryExpression) node).getLeftTerm());
        Object right = this.evaluateExpression(((BoundBinaryExpression) node).getRightTerm());
        OperatorType tokenKind = ((BoundBinaryExpression) node).getOperator().getOperatorType();

        switch(tokenKind)
        {
            case AdditionOperator:
                return (int)left + (int)right;
            case SubtractionOperator:
                return (int)left - (int)right;
            case MultiplicationOperator:
                return (int)left * (int)right;
            case DivisionOperator:
                return (int)left / (int)right;
            case LogicAndOperator:
                return (boolean)left && (boolean)right;
            case LogicOrOperator:
                return (boolean)left || (boolean)right;
            case EqualsOperator:
                return left == right;
            case NotEqualsOperator:
                return left != right;
            default:
                throw new Exception(String.format("Unexpected binary operator '%s'", tokenKind));
        }
    }
}
