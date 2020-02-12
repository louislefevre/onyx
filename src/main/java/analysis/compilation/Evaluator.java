package analysis.compilation;

import analysis.lexical.TokenType;
import analysis.syntactic.*;

public final class Evaluator
{
    private final SyntaxTree syntaxTree;

    public Evaluator(SyntaxTree syntaxTree)
    {
        this.syntaxTree = syntaxTree;
    }

    public int evaluate()
    {
        if(!errorsPresent())
        {
            try{
                return evaluateExpression(this.syntaxTree.getExpression());
            } catch(Exception error) {
                System.out.println(error.getMessage());
            }
        }
        return 0;
    }

    private boolean errorsPresent()
    {
        if(!this.syntaxTree.getDiagnosticsLog().isEmpty())
        {
            this.syntaxTree.showDiagnostics();
            return true;
        }
        return false;
    }

    private int evaluateExpression(Expression node) throws Exception
    {
        if(node instanceof LiteralExpression)
            return this.evaluateNumberExpression(node);

        if(node instanceof UnaryExpression)
            return this.evaluateUnaryExpression(node);

        if(node instanceof BinaryExpression)
            return this.evaluateBinaryExpression(node);

        if(node instanceof ParenthesizedExpression)
            return this.evaluateParenthesizedExpression(node);

        throw new Exception(String.format("Unexpected node '%s'", node.getType()));
    }

    private int evaluateNumberExpression(Expression node)
    {
        return (int) ((LiteralExpression) node).getLiteralToken().getValue();
    }

    private int evaluateUnaryExpression(Expression node) throws Exception
    {
        int operand = this.evaluateExpression(((UnaryExpression) node).getOperand());
        TokenType operatorType = ((UnaryExpression) node).getOperatorToken().getType();

        if(operatorType == TokenType.PlusToken)
            return operand;
        else if(operatorType == TokenType.MinusToken)
            return -operand;
        else
            throw new Exception(String.format("Unexpected unary operator '%s'", operatorType));
    }

    private int evaluateBinaryExpression(Expression node) throws Exception
    {
        int left = evaluateExpression(((BinaryExpression) node).getLeftTerm());
        int right = evaluateExpression(((BinaryExpression) node).getRightTerm());
        TokenType tokenKind = ((BinaryExpression) node).getOperatorToken().getType();

        switch(tokenKind)
        {
            case PlusToken:
                return left + right;
            case MinusToken:
                return left - right;
            case StarToken:
                return left * right;
            case SlashToken:
                return left / right;
            default:
                throw new Exception(String.format("Unexpected binary operator '%s'", tokenKind));
        }
    }

    private int evaluateParenthesizedExpression(Expression node) throws Exception
    {
        return evaluateExpression(((ParenthesizedExpression) node).getExpression());
    }
}
