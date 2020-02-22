package analysis.semantic;

import identifiers.OperatorType;
import identifiers.ObjectType;
import identifiers.TokenType;
import lombok.Getter;

public final class BoundBinaryOperator extends BoundOperator
{
    @Getter private final TokenType tokenType;
    @Getter private final OperatorType operatorType;
    @Getter private final ObjectType leftObjectType;
    @Getter private final ObjectType rightObjectType;
    @Getter private final ObjectType resultObjectType;

    public BoundBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType leftObjectType, ObjectType rightObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.leftObjectType = leftObjectType;
        this.rightObjectType = rightObjectType;
        this.resultObjectType = resultObjectType;
    }

    public BoundBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType operandObjectType, ObjectType resultObjectType)
    {
        this(tokenType, operatorType, operandObjectType, operandObjectType, resultObjectType);
    }

    public BoundBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType objectType)
    {
        this(tokenType, operatorType, objectType, objectType, objectType);
    }
}
