package main.java.codeanalysis;

public class Evaluator
{
    private SyntaxTree syntaxTree;

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

    private int evaluateExpression(ExpressionSyntax node) throws Exception
    {
        if(node instanceof NumberExpressionSyntax)
            return this.evaluateNumberExpression(node);

        if(node instanceof BinaryExpressionSyntax)
            return this.evaluateBinaryExpression(node);

        if(node instanceof ParenthesizedExpressionSyntax)
            return this.evaluateParenthesizedExpression(node);

        throw new Exception(String.format("Unexpected node '%s'", node.getKind()));
    }

    private int evaluateNumberExpression(ExpressionSyntax node)
    {
        return (int) ((NumberExpressionSyntax) node).getNumberToken().getValue();
    }

    private int evaluateBinaryExpression(ExpressionSyntax node) throws Exception
    {
        int left = evaluateExpression(((BinaryExpressionSyntax) node).getLeft());
        int right = evaluateExpression(((BinaryExpressionSyntax) node).getRight());
        SyntaxKind tokenKind = ((BinaryExpressionSyntax) node).getOperatorToken().getKind();

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

    private int evaluateParenthesizedExpression(ExpressionSyntax node) throws Exception
    {
        return evaluateExpression(((ParenthesizedExpressionSyntax) node).getExpression());
    }
}
