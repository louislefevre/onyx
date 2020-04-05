package analysis.semantic;

import lombok.Getter;
import identifiers.ExpressionType;
import identifiers.ObjectType;
import org.jetbrains.annotations.NotNull;

@Getter
public final class AnnotatedBinaryExpression extends AnnotatedExpression
{
    private final AnnotatedExpression leftTerm;
    private final AnnotatedBinaryOperator operator;
    private final AnnotatedExpression rightTerm;
    private final ExpressionType expressionType;
    private final ObjectType objectType;

    public AnnotatedBinaryExpression(AnnotatedExpression leftTerm, @NotNull AnnotatedBinaryOperator operator,
                                     AnnotatedExpression rightTerm)
    {
        this.leftTerm = leftTerm;
        this.operator = operator;
        this.rightTerm = rightTerm;
        this.expressionType = ExpressionType.BINARY_EXPRESSION;
        this.objectType = operator.getResultObjectType();
    }
}
