package errors;

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

    public static Error unexpectedToken(String syntax, Object type, int start, int length)
    {
        TextSpan span = new TextSpan(start, length);
        String message = String.format("ERROR: Unexpected token '%1s' of type %2s.", syntax, type);
        return new SemanticError(span, message);
    }

    public static Error unexpectedTokenMatch(String syntax, Object type, int start, int length)
    {
        TextSpan span = new TextSpan(start, length);
        String message = String.format("ERROR: Unexpected token '%1s', expected '%2s'", syntax, type);
        return new SemanticError(span, message);
    }
}
