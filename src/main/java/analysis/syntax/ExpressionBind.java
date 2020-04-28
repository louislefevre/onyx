package analysis.syntax;

import identifiers.TokenType;
import lombok.Getter;

@Getter
public final class ExpressionBind
{
    private final TokenType type;
    private final TokenType[] compatibleTypes;

    public ExpressionBind(TokenType type, TokenType[] compatibleTypes)
    {
        this.type = type;
        this.compatibleTypes = compatibleTypes;
    }
}
