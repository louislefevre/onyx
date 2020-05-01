package analysis.semantic;

import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;
import lombok.Getter;

@Getter
public final class AnnotatedIdentifierExpression implements AnnotatedExpression
{
    private final String name;
    private final ObjectType objectType;
    private final AnnotatedExpressionType expressionType;

    public AnnotatedIdentifierExpression(String name, ObjectType type)
    {
        this.name = name;
        this.objectType = type;
        this.expressionType = AnnotatedExpressionType.ANNOTATED_IDENTIFIER_EXPRESSION;
    }
}
