package errors;

import identifiers.ErrorType;
import identifiers.ObjectType;
import lombok.Getter;
import source.SourceSpan;

@Getter
public final class SemanticError extends Error
{
    private final ErrorType errorType;

    public SemanticError(SourceSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.SEMANTIC_ERROR;
    }

    public static SemanticError undefinedUnaryOperator(SourceSpan span, String syntax, ObjectType type)
    {
        String message = String.format("Unary operator '%1s' is not defined for type '%2s'.", syntax, type);
        return new SemanticError(span, message);
    }

    public static SemanticError undefinedBinaryOperator(SourceSpan span, String syntax, ObjectType leftType,
                                                        ObjectType rightType)
    {
        String message = String.format("Binary operator '%1s' is not defined for type '%2s' and '%3s'.", syntax,
                                       leftType, rightType);
        return new SemanticError(span, message);
    }

    public static SemanticError undefinedName(SourceSpan span, String syntax)
    {
        String message = String.format("Variable '%s' does not exist.", syntax);
        return new SemanticError(span, message);
    }

    public static Exception undefinedExpression(String syntax)
    {
        String message = String.format("Unexpected syntax '%s'", syntax);
        return new Exception(message);
    }

    @Override
    public String toString()
    {
        return "Semantic Error";
    }
}
