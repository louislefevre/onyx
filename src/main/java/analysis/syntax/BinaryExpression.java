package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Getter
public final class BinaryExpression extends Expression
{
    private final Expression leftTerm;
    private final Token operatorToken;
    private final Expression rightTerm;
    private final TokenType tokenType;
    private final List<Object> children;

    public BinaryExpression(Expression leftTerm, Token operatorToken, Expression rightTerm)
    {
        this.leftTerm = leftTerm;
        this.operatorToken = operatorToken;
        this.rightTerm = rightTerm;
        this.tokenType = TokenType.BINARY_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Arrays.asList(leftTerm, operatorToken, rightTerm));
    }
}
