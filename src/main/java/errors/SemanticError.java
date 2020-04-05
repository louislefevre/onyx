package errors;

import analysis.lexical.SyntaxSpan;
import identifiers.ObjectType;
import lombok.Getter;
import identifiers.ErrorType;
import org.jetbrains.annotations.NotNull;

@Getter
public final class SemanticError extends Error
{
    private final ErrorType errorType;

    public SemanticError(SyntaxSpan span, String errorMessage)
    {
        super(span, errorMessage);
        this.errorType = ErrorType.SEMANTIC_ERROR;
    }

    @NotNull
    public static SemanticError undefinedUnaryOperator(SyntaxSpan span, String syntax, ObjectType type)
    {
        String message = String.format("Unary operator '%1s' is not defined for type '%2s'.", syntax, type);
        return new SemanticError(span, message);
    }

    @NotNull
    public static SemanticError undefinedBinaryOperator(SyntaxSpan span, String syntax, ObjectType leftType,
                                                        ObjectType rightType)
    {
        String message = String.format("Binary operator '%1s' is not defined for type '%2s' and '%3s'.", syntax,
                                       leftType, rightType);
        return new SemanticError(span, message);
    }

    @NotNull
    public static SemanticError undefinedName(SyntaxSpan span, String syntax)
    {
        String message = String.format("Variable '%s' does not exist.", syntax);
        return new SemanticError(span, message);
    }
}
