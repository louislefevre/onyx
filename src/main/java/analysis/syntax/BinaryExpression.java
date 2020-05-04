package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import static identifiers.ExpressionType.BINARY_EXPRESSION;

@Getter
public final class BinaryExpression implements Expression
{
    private final Expression leftOperand;
    private final Token operatorToken;
    private final Expression rightOperand;
    private final ExpressionType expressionType;
    private final Queue<Object> children;

    public BinaryExpression(Expression leftOperand, Token operatorToken, Expression rightOperand)
    {
        this.leftOperand = leftOperand;
        this.operatorToken = operatorToken;
        this.rightOperand = rightOperand;
        this.expressionType = BINARY_EXPRESSION;
        this.children = new LinkedList<>(Arrays.asList(leftOperand, operatorToken, rightOperand));
    }
}
