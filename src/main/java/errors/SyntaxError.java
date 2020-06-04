package errors;

import source.SourceSpan;
import types.TokenType;

import static types.ErrorType.SYNTAX_ERROR;

public final class SyntaxError extends Error
{
    public SyntaxError(SourceSpan span, String errorMessage)
    {
        super(SYNTAX_ERROR, span, errorMessage);
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

    public static SyntaxError invalidStatement(SourceSpan span)
    {
        String message = "Not a statement.";
        return new SyntaxError(span, message);
    }

    public static SyntaxError expectedExpression(SourceSpan openParenSpan, SourceSpan closeParenSpan)
    {
        SourceSpan span = SourceSpan.inRange(openParenSpan.getStart(), closeParenSpan.getEnd());
        String message = "Expression expected.";
        return new SyntaxError(span, message);
    }
}
