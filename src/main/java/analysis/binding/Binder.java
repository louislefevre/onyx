package analysis.binding;

import analysis.lexical.TokenType;
import analysis.syntactic.*;
import lombok.Getter;
import org.jetbrains.annotations.Nullable;

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
            case ParenthesizedExpressionToken:
                return this.bindParenthesizedExpression((ParenthesizedExpression)syntax);
            case LiteralExpressionToken:
                return this.bindLiteralExpression((LiteralExpression)syntax);
            case UnaryExpressionToken:
                return this.bindUnaryExpression((UnaryExpression)syntax);
            case BinaryExpressionToken:
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
        BoundUnaryOperatorKind boundOperatorKind = this.bindUnaryOperatorKind(syntax.getOperatorToken().getType(), boundOperand.getType());

        if(boundOperatorKind == null)
        {
            this.diagnosticsLog.add(String.format("Unary operator '%1s' is not defined for type '%2s'.", syntax.getOperatorToken().getText(), boundOperand.getType()));
            return boundOperand;
        }

        return new BoundUnaryExpression(boundOperatorKind, boundOperand);
    }

    private BoundExpression bindBinaryExpression(BinaryExpression syntax) throws Exception
    {
        BoundExpression boundLeft = this.bindExpression(syntax.getLeftTerm());
        BoundExpression boundRight = this.bindExpression(syntax.getRightTerm());
        BoundBinaryOperatorKind boundOperatorKind =  this.bindBinaryOperatorKind(syntax.getOperatorToken().getType(), boundLeft.getType(), boundRight.getType());

        if(boundOperatorKind == null)
        {
            this.diagnosticsLog.add(String.format("Binary operator '%1s' is not defined for type '%2s' and '%3s'.", syntax.getOperatorToken().getText(), boundLeft.getType(), boundRight.getType()));
            return boundLeft;
        }

        return new BoundBinaryExpression(boundLeft, boundOperatorKind, boundRight);
    }

    @Nullable
    private BoundUnaryOperatorKind bindUnaryOperatorKind(TokenType type, Class operandType) throws Exception
    {
        if(operandType != Integer.class)
            return null;

        switch(type)
        {
            case PlusToken:
                return BoundUnaryOperatorKind.Identity;
            case MinusToken:
                return BoundUnaryOperatorKind.Negation;
            default:
                throw new Exception(String.format("Unexpected unary operator '%s'", type));
        }
    }

    @Nullable
    private BoundBinaryOperatorKind bindBinaryOperatorKind(TokenType type, Class leftType, Class rightType) throws Exception
    {
        if(leftType != Integer.class || rightType != Integer.class)
            return null;

        switch(type)
        {
            case PlusToken:
                return BoundBinaryOperatorKind.Addition;
            case MinusToken:
                return BoundBinaryOperatorKind.Subtraction;
            case StarToken:
                return BoundBinaryOperatorKind.Multiplication;
            case SlashToken:
                return BoundBinaryOperatorKind.Division;
            default:
                throw new Exception(String.format("Unexpected unary operator '%s'", type));
        }
    }
}
