package analysis.semantic;

import lombok.Getter;
import identifiers.ExpressionType;
import identifiers.ObjectType;

public final class AnnotatedBinaryExpression extends AnnotatedExpression
{
    @Getter private final AnnotatedExpression leftTerm;
    @Getter private final AnnotatedBinaryOperator operator;
    @Getter private final AnnotatedExpression rightTerm;
    @Getter private final ExpressionType expressionType;
    @Getter private final ObjectType objectType;

    public AnnotatedBinaryExpression(AnnotatedExpression leftTerm, AnnotatedBinaryOperator operator, AnnotatedExpression rightTerm)
    {
        this.leftTerm = leftTerm;
        this.operator = operator;
        this.rightTerm = rightTerm;
        this.expressionType = ExpressionType.BINARY_EXPRESSION;
        this.objectType = operator.getResultObjectType();
    }
}
