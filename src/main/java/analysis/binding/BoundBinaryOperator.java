package analysis.binding;

import analysis.lexical.TokenType;
import lombok.Getter;

public class BoundBinaryOperator
{
    @Getter private TokenType syntaxKind;
    @Getter private BoundBinaryOperatorKind kind;
    @Getter private Class leftType;
    @Getter private Class rightType;
    @Getter private Class resultType;

    private BoundBinaryOperator(TokenType syntaxKind, BoundBinaryOperatorKind kind, Class leftType, Class rightType, Class resultType)
    {
        this.syntaxKind = syntaxKind;
        this.kind = kind;
        this.leftType = leftType;
        this.rightType = rightType;
        this.resultType = resultType;
    }

    private BoundBinaryOperator(TokenType syntaxKind, BoundBinaryOperatorKind kind, Class operandType, Class resultType)
    {
        this(syntaxKind, kind, operandType, operandType, resultType);
    }

    private BoundBinaryOperator(TokenType syntaxKind, BoundBinaryOperatorKind kind, Class type)
    {
        this(syntaxKind, kind, type, type, type);
    }

    private static BoundBinaryOperator[] operators =
    {
        new BoundBinaryOperator(TokenType.PlusToken, BoundBinaryOperatorKind.Addition, Integer.class),
        new BoundBinaryOperator(TokenType.MinusToken, BoundBinaryOperatorKind.Subtraction, Integer.class),
        new BoundBinaryOperator(TokenType.StarToken, BoundBinaryOperatorKind.Multiplication, Integer.class),
        new BoundBinaryOperator(TokenType.SlashToken, BoundBinaryOperatorKind.Division, Integer.class),
        new BoundBinaryOperator(TokenType.EqualsToken, BoundBinaryOperatorKind.Equals, Integer.class, Boolean.class),
        new BoundBinaryOperator(TokenType.NotEqualsToken, BoundBinaryOperatorKind.NotEquals, Integer.class, Boolean.class),
        new BoundBinaryOperator(TokenType.AndToken, BoundBinaryOperatorKind.LogicAnd, Boolean.class),
        new BoundBinaryOperator(TokenType.OrToken, BoundBinaryOperatorKind.LogicOr, Boolean.class),
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
