package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class LiteralExpression extends Expression
{
    @Getter private final Token literalToken;
    @Getter private final Object value;
    @Getter private final TokenType tokenType;
    @Getter private final List<Object> children;

    public LiteralExpression(Token literalToken, Object value)
    {
        this.literalToken =  literalToken;
        this.value = value;
        this.tokenType = TokenType.LITERAL_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Collections.singletonList(this.literalToken));
    }
}
