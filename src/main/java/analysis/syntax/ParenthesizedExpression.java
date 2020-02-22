package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class ParenthesizedExpression extends Expression
{
    @Getter private final Expression expression;
    @Getter private final TokenType tokenType;
    @Getter private final List<Object> children;

    public ParenthesizedExpression(Token openParenthesisToken, Expression expression, Token closeParenthesisToken)
    {
        this.expression = expression;
        this.tokenType = TokenType.PARENTHESIZED_EXPRESSION_TOKEN;
        this.children = new ArrayList<>(Arrays.asList(openParenthesisToken, this.expression, closeParenthesisToken));
    }
}
