package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class UnaryExpression extends Expression
{
    @Getter private final Token operatorToken;
    @Getter private final Expression operand;
    @Getter private final TokenType tokenType;
    @Getter private final List<Object> children;

    public UnaryExpression(Token operatorToken, Expression operand)
    {
        this.operatorToken = operatorToken;
        this.operand = operand;
        this.tokenType = TokenType.UNARY_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Arrays.asList(operatorToken, operand));
    }
}
