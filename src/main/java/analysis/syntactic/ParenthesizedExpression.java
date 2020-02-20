package analysis.syntactic;

import analysis.lexical.Node;
import analysis.lexical.Token;
import analysis.identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class ParenthesizedExpression extends Expression
{
    private final Token openParenthesisToken;
    @Getter private final Expression expression;
    private final Token closeParenthesisToken;
    @Getter private final TokenType type;
    @Getter private final List<Node> children;

    public ParenthesizedExpression(Token openParenthesisToken, Expression expression, Token closeParenthesisToken)
    {
        this.openParenthesisToken = openParenthesisToken;
        this.expression = expression;
        this.closeParenthesisToken = closeParenthesisToken;
        this.type = TokenType.ParenthesizedExpression;
        this.children = new ArrayList<>(Arrays.asList(this.openParenthesisToken, this.expression, this.closeParenthesisToken));
    }
}
