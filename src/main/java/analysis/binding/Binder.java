package analysis.binding;

import analysis.syntactic.BinaryExpression;
import analysis.syntactic.Expression;
import analysis.syntactic.LiteralExpression;
import analysis.syntactic.UnaryExpression;

final class Binder
{
    public Binder() { }

    public BoundExpression bind(Expression syntax) throws Exception
    {
        switch(syntax.getType())
        {
            case LiteralExpressionToken:
                return BindLiteralExpression((LiteralExpression)syntax);
            case UnaryExpressionToken:
                return BindUnaryExpression((UnaryExpression)syntax);
            case BinaryExpressionToken:
                return BindBinaryExpression((BinaryExpression)syntax);
            default:
                throw new Exception(String.format("Unexpected syntax '%s'", syntax.getType()));
        }
    }

    private BoundExpression BindLiteralExpression(LiteralExpression syntax)
    {
        return null;
    }

    private BoundExpression BindUnaryExpression(UnaryExpression syntax)
    {
        return null;
    }

    private BoundExpression BindBinaryExpression(BinaryExpression syntax)
    {
        return null;
    }
}












