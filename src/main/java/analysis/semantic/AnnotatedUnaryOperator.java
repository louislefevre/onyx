package analysis.semantic;

import lombok.Getter;
import identifiers.ObjectType;
import identifiers.OperatorType;
import identifiers.TokenType;

public final class AnnotatedUnaryOperator extends AnnotatedOperator
{
    @Getter private final TokenType tokenType;
    @Getter private final OperatorType operatorType;
    @Getter private final ObjectType operandObjectType;
    @Getter private final ObjectType resultObjectType;

    public AnnotatedUnaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType operandObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.operandObjectType = operandObjectType;
        this.resultObjectType = resultObjectType;
    }

    public AnnotatedUnaryOperator(TokenType tokenType, OperatorType kind, ObjectType operandObjectType)
    {
        this(tokenType, kind, operandObjectType, operandObjectType);
    }
}
