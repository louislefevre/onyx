package analysis.semantic;

import symbols.OperatorType;
import symbols.ObjectType;
import symbols.TokenType;
import lombok.Getter;

public final class BoundBinaryOperator
{
    @Getter private final TokenType tokenType;
    @Getter private final OperatorType operatorType;
    @Getter private final ObjectType leftObjectType;
    @Getter private final ObjectType rightObjectType;
    @Getter private final ObjectType resultObjectType;

    private BoundBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType leftObjectType, ObjectType rightObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.leftObjectType = leftObjectType;
        this.rightObjectType = rightObjectType;
        this.resultObjectType = resultObjectType;
    }

    private BoundBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType operandObjectType, ObjectType resultObjectType)
    {
        this(tokenType, operatorType, operandObjectType, operandObjectType, resultObjectType);
    }

    private BoundBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType objectType)
    {
        this(tokenType, operatorType, objectType, objectType, objectType);
    }

    private static final BoundBinaryOperator[] operators =
    {
        new BoundBinaryOperator(TokenType.PlusToken, OperatorType.Addition, ObjectType.IntegerObject),
        new BoundBinaryOperator(TokenType.MinusToken, OperatorType.Subtraction, ObjectType.IntegerObject),
        new BoundBinaryOperator(TokenType.StarToken, OperatorType.Multiplication, ObjectType.IntegerObject),
        new BoundBinaryOperator(TokenType.SlashToken, OperatorType.Division, ObjectType.IntegerObject),
        new BoundBinaryOperator(TokenType.EqualsToken, OperatorType.Equals, ObjectType.IntegerObject, ObjectType.BooleanObject),
        new BoundBinaryOperator(TokenType.NotEqualsToken, OperatorType.NotEquals, ObjectType.IntegerObject, ObjectType.BooleanObject),
        new BoundBinaryOperator(TokenType.AndToken, OperatorType.LogicAnd, ObjectType.BooleanObject),
        new BoundBinaryOperator(TokenType.OrToken, OperatorType.LogicOr, ObjectType.BooleanObject),
    };

    public static BoundBinaryOperator bind(TokenType tokenType, ObjectType leftObjectType, ObjectType rightObjectType)
    {
        for(BoundBinaryOperator operator : operators)
        {
            if(operator.getTokenType() == tokenType && operator.getLeftObjectType() == leftObjectType && operator.getRightObjectType() == rightObjectType)
                return operator;
        }

        return null;
    }
}
