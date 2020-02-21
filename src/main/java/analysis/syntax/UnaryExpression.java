package analysis.syntax;

import analysis.lexical.Node;
import analysis.lexical.Token;
import symbols.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class UnaryExpression extends Expression
{
    @Getter private final Token operatorToken;
    @Getter private final Expression operand;
    @Getter private final TokenType tokenType;
    @Getter private final List<Node> children;

    public UnaryExpression(Token operatorToken, Expression operand)
    {
        this.operatorToken = operatorToken;
        this.operand = operand;
        this.tokenType = TokenType.UnaryExpressionToken;
        this.children = new ArrayList<>(Arrays.asList(this.operatorToken, this.operand));
    }
}
