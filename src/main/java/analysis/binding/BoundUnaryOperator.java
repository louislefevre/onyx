package analysis.binding;

import analysis.identifiers.BoundUnaryOperatorType;
import analysis.identifiers.TokenType;
import lombok.Getter;

public final class BoundUnaryOperator
{
    @Getter private TokenType syntaxKind;
    @Getter private BoundUnaryOperatorType kind;
    @Getter private Class operandType;
    @Getter private Class resultType;

    private BoundUnaryOperator(TokenType syntaxKind, BoundUnaryOperatorType kind, Class operandType, Class resultType)
    {
        this.syntaxKind = syntaxKind;
        this.kind = kind;
        this.operandType = operandType;
        this.resultType = resultType;
    }

    private BoundUnaryOperator(TokenType syntaxKind, BoundUnaryOperatorType kind, Class operandType)
    {
        this(syntaxKind, kind, operandType, operandType);
    }

    private static BoundUnaryOperator[] operators =
    {
        new BoundUnaryOperator(TokenType.BangToken, BoundUnaryOperatorType.LogicNegation, Boolean.class),
        new BoundUnaryOperator(TokenType.PlusToken, BoundUnaryOperatorType.Identity, Integer.class),
        new BoundUnaryOperator(TokenType.BangToken, BoundUnaryOperatorType.Negation, Integer.class)
    };

    public static BoundUnaryOperator bind(TokenType syntaxKind, Class operandType)
    {
        for(BoundUnaryOperator operator : operators)
        {
            if(operator.getSyntaxKind() == syntaxKind && operator.getOperandType() == operandType)
                return operator;
        }

        return null;
    }
}
