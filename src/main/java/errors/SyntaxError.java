package errors;

import analysis.lexical.SyntaxSpan;
import identifiers.TokenType;
import lombok.Getter;
import identifiers.ErrorType;

public final class SyntaxError extends Error
{
    @Getter private final ErrorType errorType;

    public SyntaxError(SyntaxSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.SYNTAX_ERROR;
    }

    public static Error unexpectedToken(SyntaxSpan span, TokenType type)
    {
        String message = String.format("Unexpected token '%s'.", type);
        return new SemanticError(span, message);
    }

    public static Error unexpectedTokenMatch(SyntaxSpan span, TokenType actualType, TokenType expectedType)
    {
        String message = String.format("Unexpected token '%1s', expected '%2s'.", actualType, expectedType);
        return new SemanticError(span, message);
    }
}
