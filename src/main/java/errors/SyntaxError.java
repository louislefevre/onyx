package errors;

import identifiers.TokenType;
import source.SourceSpan;

import static identifiers.ErrorType.SYNTAX_ERROR;

public final class SyntaxError extends Error
{
    public SyntaxError(SourceSpan span, String errorMessage)
    {
        super(span, SYNTAX_ERROR, errorMessage);
    }

    public static SyntaxError invalidToken(SourceSpan span, TokenType type)
    {
        String message = String.format("Unexpected token '%s'.", type);
        return new SyntaxError(span, message);
    }

    public static SyntaxError invalidTokenPair(SourceSpan span, TokenType actualType, TokenType expectedType)
    {
        String message = String.format("Unexpected token '%1s', expected '%2s'.", actualType, expectedType);
        return new SyntaxError(span, message);
    }
}
