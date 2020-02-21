package analysis.semantic;

import symbols.BinaryOperatorType;
import symbols.TokenType;
import lombok.Getter;

public final class BoundBinaryOperator
{
    @Getter private final TokenType tokenType;
    @Getter private final BinaryOperatorType kind;
    @Getter private final Class leftClassType;
    @Getter private final Class rightClassType;
    @Getter private final Class resultClassType;

    private BoundBinaryOperator(TokenType tokenType, BinaryOperatorType kind, Class leftClassType, Class rightClassType, Class resultClassType)
    {
        this.tokenType = tokenType;
        this.kind = kind;
        this.leftClassType = leftClassType;
        this.rightClassType = rightClassType;
        this.resultClassType = resultClassType;
    }

    private BoundBinaryOperator(TokenType tokenType, BinaryOperatorType kind, Class operandClassType, Class resultClassType)
    {
        this(tokenType, kind, operandClassType, operandClassType, resultClassType);
    }

    private BoundBinaryOperator(TokenType tokenType, BinaryOperatorType kind, Class classType)
    {
        this(tokenType, kind, classType, classType, classType);
    }

    private static final BoundBinaryOperator[] operators =
    {
        new BoundBinaryOperator(TokenType.PlusToken, BinaryOperatorType.Addition, Integer.class),
        new BoundBinaryOperator(TokenType.MinusToken, BinaryOperatorType.Subtraction, Integer.class),
        new BoundBinaryOperator(TokenType.StarToken, BinaryOperatorType.Multiplication, Integer.class),
        new BoundBinaryOperator(TokenType.SlashToken, BinaryOperatorType.Division, Integer.class),
        new BoundBinaryOperator(TokenType.EqualsToken, BinaryOperatorType.Equals, Integer.class, Boolean.class),
        new BoundBinaryOperator(TokenType.NotEqualsToken, BinaryOperatorType.NotEquals, Integer.class, Boolean.class),
        new BoundBinaryOperator(TokenType.AndToken, BinaryOperatorType.LogicAnd, Boolean.class),
        new BoundBinaryOperator(TokenType.OrToken, BinaryOperatorType.LogicOr, Boolean.class),
    };

    public static BoundBinaryOperator bind(TokenType tokenType, Class leftClassType, Class rightClassType)
    {
        for(BoundBinaryOperator operator : operators)
        {
            if(operator.getTokenType() == tokenType && operator.getLeftClassType() == leftClassType && operator.getRightClassType() == rightClassType)
                return operator;
        }

        return null;
    }
}
