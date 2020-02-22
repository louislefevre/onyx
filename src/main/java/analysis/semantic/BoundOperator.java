package analysis.semantic;

import identifiers.ObjectType;
import identifiers.OperatorType;
import identifiers.TokenType;

public abstract class BoundOperator
{
    public abstract TokenType getTokenType();
    public abstract OperatorType getOperatorType();
    public abstract ObjectType getResultObjectType();
}
