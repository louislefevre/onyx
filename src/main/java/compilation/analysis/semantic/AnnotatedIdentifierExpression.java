package compilation.analysis.semantic;

import lombok.Getter;
import types.AnnotatedExpressionType;
import types.ObjectType;

import static types.AnnotatedExpressionType.ANNOTATED_IDENTIFIER_EXPRESSION;

@Getter
public final class AnnotatedIdentifierExpression implements AnnotatedExpression
{
    private final String name;
    private final ObjectType objectType;
    private final AnnotatedExpressionType expressionType;

    public AnnotatedIdentifierExpression(String name, ObjectType objectType)
    {
        this.name = name;
        this.objectType = objectType;
        this.expressionType = ANNOTATED_IDENTIFIER_EXPRESSION;
    }
}
