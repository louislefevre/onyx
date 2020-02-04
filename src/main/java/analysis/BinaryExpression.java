package main.java.analysis;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class BinaryExpression extends Expression
{
    private Expression leftTerm;
    private Token operatorToken;
    private Expression rightTerm;

    public BinaryExpression(Expression leftTerm, Token operatorToken, Expression rightTerm)
    {
        this.leftTerm = leftTerm;
        this.operatorToken = operatorToken;
        this.rightTerm = rightTerm;
    }

    @Override
    public TokenType getType() { return TokenType.BinaryExpressionToken; }
    @Override
    public List<Node> getChildren() { return new ArrayList<>(Arrays.asList(this.leftTerm, this.operatorToken, this.rightTerm)); }
    public Expression getLeftTerm() { return this.leftTerm; }
    public Token getOperatorToken() { return this.operatorToken; }
    public Expression getRightTerm() { return this.rightTerm; }
}
