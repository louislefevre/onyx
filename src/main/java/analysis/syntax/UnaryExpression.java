package analysis.syntax;

import analysis.lexical.Token;
import identifiers.ExpressionType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Getter
public final class UnaryExpression implements Expression
{
    private final Token operatorToken;
    private final Expression operand;
    private final ExpressionType expressionType;
    private final List<Object> children;

    public UnaryExpression(Token operatorToken, Expression operand)
    {
        this.operatorToken = operatorToken;
        this.operand = operand;
        this.expressionType = ExpressionType.UNARY_EXPRESSION;
        this.children = new ArrayList<>(Arrays.asList(operatorToken, operand));
    }
}
