package analysis.semantic;

import lombok.Getter;
import symbols.ObjectType;
import symbols.OperatorType;
import symbols.TokenType;

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
        new BoundUnaryOperator(TokenType.BangToken, OperatorType.LogicNegation, ObjectType.BooleanObject),
        new BoundUnaryOperator(TokenType.PlusToken, OperatorType.Identity, ObjectType.IntegerObject),
        new BoundUnaryOperator(TokenType.MinusToken, OperatorType.Negation, ObjectType.IntegerObject)
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
