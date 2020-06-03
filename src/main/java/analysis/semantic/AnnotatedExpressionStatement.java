package analysis.semantic;

import lombok.Getter;
import types.AnnotatedStatementType;

import static types.AnnotatedStatementType.ANNOTATED_EXPRESSION_STATEMENT;

@Getter
public final class AnnotatedExpressionStatement implements AnnotatedStatement
{
    private final AnnotatedExpression expression;
    private final AnnotatedStatementType statementType;

    public AnnotatedExpressionStatement(AnnotatedExpression expression)
    {
        this.expression = expression;
        this.statementType = ANNOTATED_EXPRESSION_STATEMENT;
    }
}
