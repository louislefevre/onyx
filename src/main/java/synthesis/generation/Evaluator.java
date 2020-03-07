package synthesis.generation;

import analysis.semantic.*;
import errors.ErrorHandler;
import identifiers.OperatorType;
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

    private Object evaluate()
    {
        try
        {
            return this.evaluateExpression(this.annotatedParseTree.getExpression());
        }
        catch(Exception exception)
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

    private Object evaluateVariableExpression(AnnotatedVariableExpression node) throws Exception
    {
        String name = node.getName();
        if(this.symbolTable.containsSymbol(name))
            return this.symbolTable.getSymbol(node.getName()).getValue();
        throw new Exception(String.format("EXCEPTION: Symbol '%s' does not exist in symbol table.", name));
    }

    private Object evaluateAssignmentExpression(AnnotatedAssignmentExpression node) throws Exception
    {
        Object value = this.evaluateExpression(node.getExpression());
        this.symbolTable.addSymbol(node.getName(), value, node.getObjectType());
        return value;
    }
}
