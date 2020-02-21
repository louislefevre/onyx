package analysis.semantic;

import lombok.Getter;
import identifiers.ObjectType;
import identifiers.OperatorType;
import identifiers.TokenType;

public final class BoundUnaryOperator
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
        new BoundUnaryOperator(TokenType.BangToken, OperatorType.LogicNegationOperator, ObjectType.BooleanObject),
        new BoundUnaryOperator(TokenType.PlusToken, OperatorType.IdentityOperator, ObjectType.IntegerObject),
        new BoundUnaryOperator(TokenType.MinusToken, OperatorType.NegationOperator, ObjectType.IntegerObject)
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
