package analysis.semantic;

import identifiers.ObjectType;
import identifiers.OperatorType;
import identifiers.TokenType;
import lombok.Getter;

@Getter
public final class AnnotatedBinaryOperator extends AnnotatedOperator
{
    private final TokenType tokenType;
    private final OperatorType operatorType;
    private final ObjectType leftObjectType;
    private final ObjectType rightObjectType;
    private final ObjectType resultObjectType;

    public AnnotatedBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType leftObjectType,
                                   ObjectType rightObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.leftObjectType = leftObjectType;
        this.rightObjectType = rightObjectType;
        this.resultObjectType = resultObjectType;
    }

    public AnnotatedBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType operandObjectType,
                                   ObjectType resultObjectType)
    {
        this(tokenType, operatorType, operandObjectType, operandObjectType, resultObjectType);
    }

    public AnnotatedBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType objectType)
    {
        this(tokenType, operatorType, objectType, objectType, objectType);
    }

    public ObjectType getResultObjectType()
    {
        return resultObjectType;
    }
}
