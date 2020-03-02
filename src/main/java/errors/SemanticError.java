package errors;

import analysis.lexical.SyntaxSpan;
import identifiers.ObjectType;
import lombok.Getter;
import identifiers.ErrorType;

public final class SemanticError extends Error
{
    @Getter private final ErrorType errorType;

    public SemanticError(SyntaxSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.SEMANTIC_ERROR;
    }

    public static Error undefinedUnaryOperator(SyntaxSpan span, String syntax, ObjectType type)
    {
        String message = String.format("Unary operator '%1s' is not defined for type '%2s'.", syntax, type);
        return new SemanticError(span, message);
    }

    public static Error undefinedBinaryOperator(SyntaxSpan span, String syntax, ObjectType leftType, ObjectType rightType)
    {
        String message = String.format("Binary operator '%1s' is not defined for type '%2s' and '%3s'.", syntax, leftType, rightType);
        return new SemanticError(span, message);
    }


}
