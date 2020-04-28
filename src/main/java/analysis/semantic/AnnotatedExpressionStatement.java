package analysis.semantic;

import identifiers.AnnotatedStatementType;
import lombok.Getter;

@Getter
public final class AnnotatedExpressionStatement extends AnnotatedStatement
{
    private final AnnotatedExpression annotatedExpression;
    private final AnnotatedStatementType annotatedStatementType;

    public AnnotatedExpressionStatement(AnnotatedExpression annotatedExpression)
    {
        this.annotatedExpression = annotatedExpression;
        this.annotatedStatementType = AnnotatedStatementType.ANNOTATED_EXPRESSION_STATEMENT;
    }
}
