package analysis.syntactic;

import analysis.lexical.Node;
import analysis.lexical.Token;
import analysis.lexical.TokenType;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class NumberExpression extends Expression
{
    private final Token numberToken;

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
        return new ArrayList<>(Collections.singletonList(this.numberToken));
    }

    public Token getNumberToken()
    {
        return this.numberToken;
    }
}
