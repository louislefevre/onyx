package analysis.binding;

import analysis.syntactic.*;
import lombok.Getter;

import java.util.List;

public final class Binder
{
    @Getter private final List<String> diagnosticsLog;
    @Getter private final BoundExpression syntaxTree;

    public Binder(SyntaxTree syntaxTree)
    {
        this.diagnosticsLog = syntaxTree.getDiagnosticsLog();
        this.syntaxTree = this.bind(syntaxTree.getExpression());
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
        switch(syntax.getType())
        {
            case ParenthesizedExpression:
                return this.bindParenthesizedExpression((ParenthesizedExpression)syntax);
            case LiteralExpression:
                return this.bindLiteralExpression((LiteralExpression)syntax);
            case UnaryExpression:
                return this.bindUnaryExpression((UnaryExpression)syntax);
            case BinaryExpression:
                return this.bindBinaryExpression((BinaryExpression)syntax);
            default:
                throw new Exception(String.format("Unexpected syntax '%s'", syntax.getType()));
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
        BoundUnaryOperator boundOperator = BoundUnaryOperator.bind(syntax.getOperatorToken().getType(), boundOperand.getType());

        if(boundOperator == null)
        {
            this.diagnosticsLog.add(String.format("Unary operator '%1s' is not defined for type '%2s'.", syntax.getOperatorToken().getText(), boundOperand.getType()));
            return boundOperand;
        }

        return new BoundUnaryExpression(boundOperator, boundOperand);
    }

    private BoundExpression bindBinaryExpression(BinaryExpression syntax) throws Exception
    {
        BoundExpression boundLeft = this.bindExpression(syntax.getLeftTerm());
        BoundExpression boundRight = this.bindExpression(syntax.getRightTerm());
        BoundBinaryOperator boundOperator = BoundBinaryOperator.bind(syntax.getOperatorToken().getType(), boundLeft.getType(), boundRight.getType());

        if(boundOperator == null)
        {
            this.diagnosticsLog.add(String.format("Binary operator '%1s' is not defined for type '%2s' and '%3s'.", syntax.getOperatorToken().getText(), boundLeft.getType(), boundRight.getType()));
            return boundLeft;
        }

        return new BoundBinaryExpression(boundLeft, boundOperator, boundRight);
    }
}
