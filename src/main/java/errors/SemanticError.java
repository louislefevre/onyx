package errors;

import lombok.Getter;
import identifiers.ErrorType;

public final class SemanticError extends Error
{
    @Getter private final ErrorType errorType;

    public SemanticError(TextSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.SEMANTIC_ERROR;
    }

    public static Error undefinedUnaryOperator(String syntax, Object type, int start, int length)
    {
        TextSpan span = new TextSpan(start, length);
        String message = String.format("Unary operator '%1s' is not defined for type '%2s'.", syntax, type);
        return new SemanticError(span, message);
    }

    public static Error undefinedBinaryOperator(String syntax, Object leftType, Object rightType, int start, int length)
    {
        TextSpan span = new TextSpan(start, length);
        String message = String.format("Binary operator '%1s' is not defined for type '%2s' and '%3s'.", syntax, leftType, rightType);
        return new SemanticError(span, message);
    }


}
