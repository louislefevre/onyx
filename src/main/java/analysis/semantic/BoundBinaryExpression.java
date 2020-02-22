package analysis.semantic;

import lombok.Getter;
import identifiers.ExpressionType;
import identifiers.ObjectType;

public final class BoundBinaryExpression extends BoundExpression
{
    @Getter private final BoundExpression leftTerm;
    @Getter private final BoundBinaryOperator operator;
    @Getter private final BoundExpression rightTerm;
    @Getter private final ExpressionType expressionType;
    @Getter private final ObjectType objectType;

    public BoundBinaryExpression(BoundExpression leftTerm, BoundBinaryOperator operator, BoundExpression rightTerm)
    {
        this.leftTerm = leftTerm;
        this.operator = operator;
        this.rightTerm = rightTerm;
        this.expressionType = ExpressionType.BINARY_EXPRESSION;
        this.objectType = this.operator.getResultObjectType();
    }
}
