package analysis.syntax;

import analysis.lexical.Node;
import analysis.lexical.Token;
import symbols.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class BinaryExpression extends Expression
{
    @Getter private final Expression leftTerm;
    @Getter private final Token operatorToken;
    @Getter private final Expression rightTerm;
    @Getter private final TokenType tokenType;
    @Getter private final List<Node> children;

    public BinaryExpression(Expression leftTerm, Token operatorToken, Expression rightTerm)
    {
        this.leftTerm = leftTerm;
        this.operatorToken = operatorToken;
        this.rightTerm = rightTerm;
        this.tokenType = TokenType.BinaryExpression;
        this.children = new ArrayList<>(Arrays.asList(this.leftTerm, this.operatorToken, this.rightTerm));
    }
}
