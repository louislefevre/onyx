package analysis.semantic;

import symbols.UnaryOperatorType;
import symbols.TokenType;
import lombok.Getter;

public final class BoundUnaryOperator
{
    @Getter private final TokenType tokenType;
    @Getter private final UnaryOperatorType kind;
    @Getter private final Class operandClassType;
    @Getter private final Class resultClassType;

    private BoundUnaryOperator(TokenType tokenType, UnaryOperatorType kind, Class operandClassType, Class resultClassType)
    {
        this.tokenType = tokenType;
        this.kind = kind;
        this.operandClassType = operandClassType;
        this.resultClassType = resultClassType;
    }

    private BoundUnaryOperator(TokenType tokenType, UnaryOperatorType kind, Class operandClassType)
    {
        this(tokenType, kind, operandClassType, operandClassType);
    }

    private static final BoundUnaryOperator[] operators =
    {
        new BoundUnaryOperator(TokenType.BangToken, UnaryOperatorType.LogicNegation, Boolean.class),
        new BoundUnaryOperator(TokenType.PlusToken, UnaryOperatorType.Identity, Integer.class),
        new BoundUnaryOperator(TokenType.BangToken, UnaryOperatorType.Negation, Integer.class)
    };

    public static BoundUnaryOperator bind(TokenType tokenType, Class operandClassType)
    {
        for(BoundUnaryOperator operator : operators)
        {
            if(operator.getTokenType() == tokenType && operator.getOperandClassType() == operandClassType)
                return operator;
        }

        return null;
    }
}
