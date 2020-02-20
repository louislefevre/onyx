package analysis.syntactic;

import analysis.lexical.Node;
import analysis.lexical.Token;
import analysis.identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class UnaryExpression extends Expression
{
    @Getter private Token operatorToken;
    @Getter private Expression operand;
    @Getter private TokenType type;
    @Getter private List<Node> children;

    public UnaryExpression(Token operatorToken, Expression operand)
    {
        this.operatorToken = operatorToken;
        this.operand = operand;
        this.type = TokenType.UnaryExpression;
        this.children = new ArrayList<>(Arrays.asList(this.operatorToken, this.operand));
    }
}
