package analysis.binding;

import analysis.identifiers.BoundBinaryOperatorType;
import analysis.identifiers.TokenType;
import lombok.Getter;

public class BoundBinaryOperator
{
    @Getter private TokenType syntaxKind;
    @Getter private BoundBinaryOperatorType kind;
    @Getter private Class leftType;
    @Getter private Class rightType;
    @Getter private Class resultType;

    private BoundBinaryOperator(TokenType syntaxKind, BoundBinaryOperatorType kind, Class leftType, Class rightType, Class resultType)
    {
        this.syntaxKind = syntaxKind;
        this.kind = kind;
        this.leftType = leftType;
        this.rightType = rightType;
        this.resultType = resultType;
    }

    private BoundBinaryOperator(TokenType syntaxKind, BoundBinaryOperatorType kind, Class operandType, Class resultType)
    {
        this(syntaxKind, kind, operandType, operandType, resultType);
    }

    private BoundBinaryOperator(TokenType syntaxKind, BoundBinaryOperatorType kind, Class type)
    {
        this(syntaxKind, kind, type, type, type);
    }

    private static BoundBinaryOperator[] operators =
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

    public static BoundBinaryOperator bind(TokenType syntaxKind, Class leftType, Class rightType)
    {
        for(BoundBinaryOperator operator : operators)
        {
            if(operator.getSyntaxKind() == syntaxKind && operator.getLeftType() == leftType && operator.getRightType() == rightType)
                return operator;
        }

        return null;
    }
}
