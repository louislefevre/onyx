package analysis.semantic;

import analysis.syntax.*;
import errors.ErrorHandler;

public final class Binder
{
    private final ParseTree parseTree;

    public Binder(Parser parser)
    {
        this.parseTree = parser.getParseTree();
    }

    public AnnotatedParseTree getAnnotatedParseTree()
    {
        return new AnnotatedParseTree(this.getBoundExpression());
    }

    private BoundExpression getBoundExpression()
    {
        return this.bind(this.parseTree.getExpression());
    }

    private BoundExpression bind(Expression syntax)
    {
        try
        {
            return this.bindExpression(syntax);
        }
        catch(Exception error)
        {
            System.out.println(error.getMessage());
            return null;
        }
    }

    private BoundExpression bindExpression(Expression syntax) throws Exception
    {
        switch(syntax.getTokenType())
        {
            case PARENTHESIZED_EXPRESSION_TOKEN:
                return this.bindParenthesizedExpression((ParenthesizedExpression)syntax);
            case LITERAL_EXPRESSION_TOKEN:
                return this.bindLiteralExpression((LiteralExpression)syntax);
            case UNARY_EXPRESSION_TOKEN:
                return this.bindUnaryExpression((UnaryExpression)syntax);
            case BINARY_EXPRESSION_TOKEN:
                return this.bindBinaryExpression((BinaryExpression)syntax);
            default:
                throw new Exception(String.format("Unexpected syntax '%s'", syntax.getTokenType()));
        }
    }

    private BoundExpression bindParenthesizedExpression(ParenthesizedExpression syntax) throws Exception
    {
        return this.bindExpression(syntax.getExpression());
    }

    private BoundExpression bindLiteralExpression(LiteralExpression syntax)
    {
        Object value = syntax.getValue();
        if(value == null)
            value = 0;

        return new BoundLiteralExpression(value);
    }

    private BoundExpression bindUnaryExpression(UnaryExpression syntax) throws Exception
    {
        BoundExpression boundOperand = this.bindExpression(syntax.getOperand());
        BoundUnaryOperator boundOperator = BoundUnaryOperator.bind(syntax.getOperatorToken().getTokenType(), boundOperand.getObjectType());

        if(boundOperator == null)
        {
            ErrorHandler.addSemanticError(String.format("Unary operator '%1s' is not defined for type '%2s'.", syntax.getOperatorToken().getSyntax(), boundOperand.getObjectType()));
            return boundOperand;
        }

        return new BoundUnaryExpression(boundOperator, boundOperand);
    }

    private BoundExpression bindBinaryExpression(BinaryExpression syntax) throws Exception
    {
        BoundExpression boundLeft = this.bindExpression(syntax.getLeftTerm());
        BoundExpression boundRight = this.bindExpression(syntax.getRightTerm());
        BoundBinaryOperator boundOperator = BoundBinaryOperator.bind(syntax.getOperatorToken().getTokenType(), boundLeft.getObjectType(), boundRight.getObjectType());

        if(boundOperator == null)
        {
            ErrorHandler.addSemanticError(String.format("Binary operator '%1s' is not defined for type '%2s' and '%3s'.", syntax.getOperatorToken().getSyntax(), boundLeft.getObjectType(), boundRight.getObjectType()));
            return boundLeft;
        }

        return new BoundBinaryExpression(boundLeft, boundOperator, boundRight);
    }
}
