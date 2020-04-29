package analysis.semantic;

import identifiers.ObjectType;
import identifiers.OperatorType;
import identifiers.TokenType;

public interface AnnotatedOperator
{
    TokenType getTokenType();

    OperatorType getOperatorType();

    ObjectType getResultObjectType();
}
