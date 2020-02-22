package analysis.semantic;

import lombok.Getter;
import identifiers.ObjectType;
import identifiers.OperatorType;
import identifiers.TokenType;

public final class BoundUnaryOperator extends BoundOperator
{
    @Getter private final TokenType tokenType;
    @Getter private final OperatorType operatorType;
    @Getter private final ObjectType operandObjectType;
    @Getter private final ObjectType resultObjectType;

    public BoundUnaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType operandObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.operandObjectType = operandObjectType;
        this.resultObjectType = resultObjectType;
    }

    public BoundUnaryOperator(TokenType tokenType, OperatorType kind, ObjectType operandObjectType)
    {
        this(tokenType, kind, operandObjectType, operandObjectType);
    }
}
