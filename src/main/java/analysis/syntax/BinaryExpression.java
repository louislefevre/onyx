package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;
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
    @Getter private final List<Object> children;

    public BinaryExpression(Expression leftTerm, Token operatorToken, Expression rightTerm)
    {
        this.leftTerm = leftTerm;
        this.operatorToken = operatorToken;
        this.rightTerm = rightTerm;
        this.tokenType = TokenType.BINARY_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Arrays.asList(leftTerm, operatorToken, rightTerm));
    }
}
