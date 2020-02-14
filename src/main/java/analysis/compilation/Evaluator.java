package analysis.compilation;

import analysis.binding.*;

import java.util.List;

public final class Evaluator
{
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_RED = "\u001B[31m";
    private final BoundExpression syntaxTree;
    private final List<String> diagnosticsLog;

    public Evaluator(Binder binder)
    {
        this.syntaxTree = binder.getSyntaxTree();
        this.diagnosticsLog = binder.getDiagnosticsLog();
    }

    public int evaluate()
    {
        if(!errorsPresent())
        {
            try
            {
                return evaluateExpression(this.syntaxTree);
            }
            catch(Exception error)
            {
                System.out.println(error.getMessage());
            }
        }
        return 0;
    }

    private boolean errorsPresent()
    {
        if(!this.diagnosticsLog.isEmpty())
        {
            this.showDiagnostics();
            return true;
        }
        return false;
    }

    public void showDiagnostics()
    {
        for (String diagnostic : this.diagnosticsLog)
            System.out.println(ANSI_RED + diagnostic + ANSI_RESET);
    }

    private int evaluateExpression(BoundExpression node) throws Exception
    {
        if(node instanceof BoundLiteralExpression)
            return this.evaluateNumberExpression(node);

        if(node instanceof BoundUnaryExpression)
            return this.evaluateUnaryExpression(node);

        if(node instanceof BoundBinaryExpression)
            return this.evaluateBinaryExpression(node);

        throw new Exception(String.format("Unexpected node '%s'", node.getType()));
    }

    private int evaluateNumberExpression(BoundExpression node)
    {
        return (int) ((BoundLiteralExpression) node).getValue();
    }

    private int evaluateUnaryExpression(BoundExpression node) throws Exception
    {
        int operand = this.evaluateExpression(((BoundUnaryExpression) node).getOperand());
        BoundUnaryOperatorKind operatorType = ((BoundUnaryExpression) node).getOperatorKind();

        switch(operatorType)
        {
            case Identity:
                return operand;
            case Negation:
                return -operand;
            default:
                throw new Exception(String.format("Unexpected unary operator '%s'", operatorType));
        }
    }

    private int evaluateBinaryExpression(BoundExpression node) throws Exception
    {
        int left = this.evaluateExpression(((BoundBinaryExpression) node).getLeft());
        int right = this.evaluateExpression(((BoundBinaryExpression) node).getRight());
        BoundBinaryOperatorKind tokenKind = ((BoundBinaryExpression) node).getOperatorKind();

        switch(tokenKind)
        {
            case Addition:
                return left + right;
            case Subtraction:
                return left - right;
            case Multiplication:
                return left * right;
            case Division:
                return left / right;
            default:
                throw new Exception(String.format("Unexpected binary operator '%s'", tokenKind));
        }
    }
}
