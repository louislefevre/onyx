package compilation.analysis.semantic;

import types.ObjectType;
import types.OperatorType;
import types.TokenType;

public interface AnnotatedOperator
{
    TokenType getTokenType();

    OperatorType getOperatorType();

    ObjectType getResultObjectType();
}
