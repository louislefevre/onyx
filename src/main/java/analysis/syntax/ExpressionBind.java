package analysis.syntax;

import identifiers.TokenType;
import lombok.Getter;

@Deprecated
@Getter
public final class ExpressionBind
{
    private final TokenType tokenType;
    private final TokenType[] compatibleTypes;

    public ExpressionBind(TokenType tokenType, TokenType[] compatibleTypes)
    {
        this.tokenType = tokenType;
        this.compatibleTypes = compatibleTypes;
    }
}
