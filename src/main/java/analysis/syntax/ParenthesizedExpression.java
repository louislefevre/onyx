package analysis.syntax;

import analysis.lexical.Node;
import analysis.lexical.Token;
import symbols.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class ParenthesizedExpression extends Expression
{
    @Getter private final Expression expression;
    @Getter private final TokenType tokenType;
    @Getter private final List<Node> children;

    public ParenthesizedExpression(Token openParenthesisToken, Expression expression, Token closeParenthesisToken)
    {
        this.expression = expression;
        this.tokenType = TokenType.ParenthesizedExpression;
        this.children = new ArrayList<>(Arrays.asList(openParenthesisToken, this.expression, closeParenthesisToken));
    }
}
