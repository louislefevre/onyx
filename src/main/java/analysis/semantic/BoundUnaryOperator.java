package analysis.semantic;

import symbols.ObjectType;
import symbols.UnaryOperatorType;
import symbols.TokenType;
import lombok.Getter;

public final class BoundUnaryOperator
{
    @Getter private final TokenType tokenType;
    @Getter private final UnaryOperatorType operatorType;
    @Getter private final ObjectType operandObjectType;
    @Getter private final ObjectType resultObjectType;

    private BoundUnaryOperator(TokenType tokenType, UnaryOperatorType operatorType, ObjectType operandObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.operandObjectType = operandObjectType;
        this.resultObjectType = resultObjectType;
    }

    private BoundUnaryOperator(TokenType tokenType, UnaryOperatorType kind, ObjectType operandObjectType)
    {
        this(tokenType, kind, operandObjectType, operandObjectType);
    }

    private static final BoundUnaryOperator[] operators =
    {
        new BoundUnaryOperator(TokenType.BangToken, UnaryOperatorType.LogicNegation, ObjectType.BooleanObject),
        new BoundUnaryOperator(TokenType.PlusToken, UnaryOperatorType.Identity, ObjectType.IntegerObject),
        new BoundUnaryOperator(TokenType.BangToken, UnaryOperatorType.Negation, ObjectType.IntegerObject)
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
