package analysis.syntax;

import identifiers.TokenType;
import lombok.Getter;

@Getter
public class ExpressionBind
{
    private TokenType type;
    private TokenType[] compatibleTypes;

    public ExpressionBind(TokenType type, TokenType[] compatibleTypes)
    {
        this.type = type;
        this.compatibleTypes = compatibleTypes;
    }
}
