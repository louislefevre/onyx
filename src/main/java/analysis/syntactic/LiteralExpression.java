package analysis.syntactic;

import analysis.lexical.Node;
import analysis.lexical.Token;
import analysis.lexical.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class LiteralExpression extends Expression
{
    @Getter private final Token literalToken;
    @Getter private final TokenType type;
    @Getter private final List<Node> children;

    public LiteralExpression(Token literalToken)
    {
        this.literalToken =  literalToken;
        this.type = TokenType.LiteralExpressionToken;
        this.children = new ArrayList<>(Collections.singletonList(this.literalToken));
    }
}
