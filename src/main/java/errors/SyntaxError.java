package errors;

import identifiers.TokenType;
import lombok.Getter;
import identifiers.ErrorType;

public final class SyntaxError extends Error
{
    @Getter private final ErrorType errorType;

    public SyntaxError(TextSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.SYNTAX_ERROR;
    }

    public static Error unexpectedToken(TextSpan span, TokenType type)
    {
        String message = String.format("ERROR: Unexpected token '%s'.", type);
        return new SemanticError(span, message);
    }

    public static Error unexpectedTokenMatch(TextSpan span, TokenType actualType, TokenType expectedType)
    {
        String message = String.format("ERROR: Unexpected token '%1s', expected '%2s'.", actualType, expectedType);
        return new SemanticError(span, message);
    }
}
