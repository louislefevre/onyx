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
    private final Expression leftTerm;
    private final Token operatorToken;
    private final Expression rightTerm;
    private final ExpressionType expressionType;
    private final List<Object> children;

    public BinaryExpression(Expression leftTerm, Token operatorToken, Expression rightTerm)
    {
        this.leftTerm = leftTerm;
        this.operatorToken = operatorToken;
        this.rightTerm = rightTerm;
        this.expressionType = ExpressionType.BINARY_EXPRESSION;
        this.children = new ArrayList<>(Arrays.asList(leftTerm, operatorToken, rightTerm));
    }
}
