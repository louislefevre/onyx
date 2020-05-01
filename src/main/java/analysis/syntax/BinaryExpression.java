package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Getter
public final class BinaryExpression implements Expression
{
    private final Expression leftOperand;
    private final Token operatorToken;
    private final Expression rightOperand;
    private final ExpressionType expressionType;
    private final List<Object> children;

    public BinaryExpression(Expression leftOperand, Token operatorToken, Expression rightOperand)
    {
        this.leftOperand = leftOperand;
        this.operatorToken = operatorToken;
        this.rightOperand = rightOperand;
        this.expressionType = ExpressionType.BINARY_EXPRESSION;
        this.children = new ArrayList<>(Arrays.asList(leftOperand, operatorToken, rightOperand));
    }
}
