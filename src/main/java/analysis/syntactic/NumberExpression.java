package analysis.syntactic;

import analysis.lexical.Node;
import analysis.lexical.Token;
import analysis.lexical.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class NumberExpression extends Expression
{
    @Getter private final Token numberToken;
    @Getter private final TokenType type;
    @Getter private final List<Node> children;

    public NumberExpression(Token numberToken)
    {
        this.numberToken =  numberToken;
        this.type = TokenType.NumberExpressionToken;
        this.children = new ArrayList<>(Collections.singletonList(this.numberToken));
    }
}
