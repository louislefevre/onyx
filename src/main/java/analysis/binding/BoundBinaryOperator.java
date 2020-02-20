package analysis.binding;

import analysis.identifiers.BoundBinaryOperatorType;
import analysis.identifiers.TokenType;
import lombok.Getter;

public final class BoundBinaryOperator
{
    @Getter private final TokenType tokenType;
    @Getter private final BoundBinaryOperatorType kind;
    @Getter private final Class leftClassType;
    @Getter private final Class rightClassType;
    @Getter private final Class resultClassType;

    private BoundBinaryOperator(TokenType tokenType, BoundBinaryOperatorType kind, Class leftClassType, Class rightClassType, Class resultClassType)
    {
        this.tokenType = tokenType;
        this.kind = kind;
        this.leftClassType = leftClassType;
        this.rightClassType = rightClassType;
        this.resultClassType = resultClassType;
    }

    private BoundBinaryOperator(TokenType tokenType, BoundBinaryOperatorType kind, Class operandClassType, Class resultClassType)
    {
        this(tokenType, kind, operandClassType, operandClassType, resultClassType);
    }

    private BoundBinaryOperator(TokenType tokenType, BoundBinaryOperatorType kind, Class classType)
    {
        this(tokenType, kind, classType, classType, classType);
    }

    private static final BoundBinaryOperator[] operators =
    {
        new BoundBinaryOperator(TokenType.PlusToken, BoundBinaryOperatorType.Addition, Integer.class),
        new BoundBinaryOperator(TokenType.MinusToken, BoundBinaryOperatorType.Subtraction, Integer.class),
        new BoundBinaryOperator(TokenType.StarToken, BoundBinaryOperatorType.Multiplication, Integer.class),
        new BoundBinaryOperator(TokenType.SlashToken, BoundBinaryOperatorType.Division, Integer.class),
        new BoundBinaryOperator(TokenType.EqualsToken, BoundBinaryOperatorType.Equals, Integer.class, Boolean.class),
        new BoundBinaryOperator(TokenType.NotEqualsToken, BoundBinaryOperatorType.NotEquals, Integer.class, Boolean.class),
        new BoundBinaryOperator(TokenType.AndToken, BoundBinaryOperatorType.LogicAnd, Boolean.class),
        new BoundBinaryOperator(TokenType.OrToken, BoundBinaryOperatorType.LogicOr, Boolean.class),
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
