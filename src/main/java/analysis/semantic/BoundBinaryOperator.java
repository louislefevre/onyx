package analysis.semantic;

import symbols.BinaryOperatorType;
import symbols.ObjectType;
import symbols.TokenType;
import lombok.Getter;

public final class BoundBinaryOperator
{
    @Getter private final TokenType tokenType;
    @Getter private final BinaryOperatorType operatorType;
    @Getter private final ObjectType leftObjectType;
    @Getter private final ObjectType rightObjectType;
    @Getter private final ObjectType resultObjectType;

    private BoundBinaryOperator(TokenType tokenType, BinaryOperatorType operatorType, ObjectType leftObjectType, ObjectType rightObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.leftObjectType = leftObjectType;
        this.rightObjectType = rightObjectType;
        this.resultObjectType = resultObjectType;
    }

    private BoundBinaryOperator(TokenType tokenType, BinaryOperatorType operatorType, ObjectType operandObjectType, ObjectType resultObjectType)
    {
        this(tokenType, operatorType, operandObjectType, operandObjectType, resultObjectType);
    }

    private BoundBinaryOperator(TokenType tokenType, BinaryOperatorType operatorType, ObjectType objectType)
    {
        this(tokenType, operatorType, objectType, objectType, objectType);
    }

    private static final BoundBinaryOperator[] operators =
    {
        new BoundBinaryOperator(TokenType.PlusToken, BinaryOperatorType.Addition, ObjectType.IntegerObject),
        new BoundBinaryOperator(TokenType.MinusToken, BinaryOperatorType.Subtraction, ObjectType.IntegerObject),
        new BoundBinaryOperator(TokenType.StarToken, BinaryOperatorType.Multiplication, ObjectType.IntegerObject),
        new BoundBinaryOperator(TokenType.SlashToken, BinaryOperatorType.Division, ObjectType.IntegerObject),
        new BoundBinaryOperator(TokenType.EqualsToken, BinaryOperatorType.Equals, ObjectType.IntegerObject, ObjectType.BooleanObject),
        new BoundBinaryOperator(TokenType.NotEqualsToken, BinaryOperatorType.NotEquals, ObjectType.IntegerObject, ObjectType.BooleanObject),
        new BoundBinaryOperator(TokenType.AndToken, BinaryOperatorType.LogicAnd, ObjectType.BooleanObject),
        new BoundBinaryOperator(TokenType.OrToken, BinaryOperatorType.LogicOr, ObjectType.BooleanObject),
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
