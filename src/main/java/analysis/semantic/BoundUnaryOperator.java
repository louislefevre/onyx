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

    private BoundUnaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType operandObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.operandObjectType = operandObjectType;
        this.resultObjectType = resultObjectType;
    }

    private BoundUnaryOperator(TokenType tokenType, OperatorType kind, ObjectType operandObjectType)
    {
        this(tokenType, kind, operandObjectType, operandObjectType);
    }

    private static final BoundUnaryOperator[] operators =
    {
        new BoundUnaryOperator(TokenType.NOT_TOKEN, OperatorType.LOGIC_NEGATION_OPERATOR, ObjectType.BOOLEAN_OBJECT),
        new BoundUnaryOperator(TokenType.PLUS_TOKEN, OperatorType.IDENTITY_OPERATOR, ObjectType.INTEGER_OBJECT),
        new BoundUnaryOperator(TokenType.MINUS_TOKEN, OperatorType.NEGATION_OPERATOR, ObjectType.INTEGER_OBJECT)
    };

    public static BoundUnaryOperator bind(TokenType tokenType, ObjectType operandObjectType)
    {
        for(BoundUnaryOperator operator : operators)
        {
            if(operator.getTokenType() == tokenType && operator.getOperandObjectType() == operandObjectType)
                return operator;
        }

        return null;
    }
}
