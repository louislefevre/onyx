package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Getter
public final class ParenthesizedExpression extends Expression
{
    private final Expression expression;
    private final TokenType tokenType;
    private final List<Object> children;

    public ParenthesizedExpression(Token openParenthesisToken, Expression expression, Token closeParenthesisToken)
    {
        this.expression = expression;
        this.tokenType = TokenType.PARENTHESIZED_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Arrays.asList(openParenthesisToken, expression, closeParenthesisToken));
    }
}
