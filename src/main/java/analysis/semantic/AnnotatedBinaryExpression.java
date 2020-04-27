package analysis.semantic;

import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;
import lombok.Getter;

@Getter
public final class AnnotatedBinaryExpression extends AnnotatedExpression
{
    private final AnnotatedExpression leftTerm;
    private final AnnotatedBinaryOperator operator;
    private final AnnotatedExpression rightTerm;
    private final AnnotatedExpressionType annotatedExpressionType;
    private final ObjectType objectType;

    public AnnotatedBinaryExpression(AnnotatedExpression leftTerm, AnnotatedBinaryOperator operator,
                                     AnnotatedExpression rightTerm)
    {
        this.leftTerm = leftTerm;
        this.operator = operator;
        this.rightTerm = rightTerm;
        this.annotatedExpressionType = AnnotatedExpressionType.ANNOTATED_BINARY_EXPRESSION;
        this.objectType = operator.getResultObjectType();
    }
}
