package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Getter
public final class NameExpression extends Expression
{
    private final Token identifierToken;
    private final TokenType tokenType;
    private final List<Object> children;

    public NameExpression(Token identifierToken)
    {
        this.identifierToken = identifierToken;
        this.tokenType = TokenType.NAME_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Collections.singletonList(identifierToken));
    }
}
