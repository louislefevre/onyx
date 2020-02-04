package main.java.analysis;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class NumberExpression extends Expression
{
    private Token numberToken;

    public NumberExpression(Token numberToken)
    {
        this.numberToken =  numberToken;
    }

    @Override
    public TokenType getType()
    {
        return TokenType.NumberExpressionToken;
    }

    @Override
    public List<Node> getChildren()
    {
        return new ArrayList<>(Arrays.asList(this.numberToken));
    }

    public Token getNumberToken()
    {
        return this.numberToken;
    }
}
