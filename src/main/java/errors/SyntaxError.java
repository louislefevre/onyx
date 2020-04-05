package errors;

import analysis.lexical.SyntaxSpan;
import identifiers.TokenType;
import lombok.Getter;
import identifiers.ErrorType;
import org.jetbrains.annotations.NotNull;

@Getter
public final class SyntaxError extends Error
{
    private final ErrorType errorType;

    public SyntaxError(SyntaxSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.SYNTAX_ERROR;
    }

    @NotNull
    public static SyntaxError unexpectedToken(SyntaxSpan span, TokenType type)
    {
        String message = String.format("Unexpected token '%s'.", type);
        return new SyntaxError(span, message);
    }

    @NotNull
    public static SyntaxError unexpectedTokenMatch(SyntaxSpan span, TokenType actualType, TokenType expectedType)
    {
        String message = String.format("Unexpected token '%1s', expected '%2s'.", actualType, expectedType);
        return new SyntaxError(span, message);
    }
}
