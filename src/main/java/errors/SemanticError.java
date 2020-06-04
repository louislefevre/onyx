package errors;

import source.SourceSpan;
import types.ObjectType;

import static types.ErrorType.SEMANTIC_ERROR;

public final class SemanticError extends Error
{
    public SemanticError(SourceSpan span, String errorMessage)
    {
        super(SEMANTIC_ERROR, span, errorMessage);
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

    public static SemanticError undefinedAssignmentOperator(SourceSpan span, String syntax, ObjectType symbolType,
                                                            ObjectType assignmentType)
    {
        String message = String.format("Assignment operator '%1s' is not defined for type '%2s' and '%3s'.", syntax,
                                       symbolType, assignmentType);
        return new SemanticError(span, message);
    }

    public static SemanticError undefinedIdentifier(SourceSpan span, String syntax)
    {
        String message = String.format("Variable '%s' does not exist.", syntax);
        return new SemanticError(span, message);
    }

    public static SemanticError invalidExpressionTypes(SourceSpan span, ObjectType actualType, ObjectType[] targetTypes)
    {
        String message = String.format("Invalid expression type '%1s', expected '%2s'.", actualType, typesToString(targetTypes));
        return new SemanticError(span, message);
    }

    private static String typesToString(ObjectType[] types)
    {
        ObjectType last = types[0];
        for (ObjectType type : types)
            last = type;

        StringBuilder builder = new StringBuilder();
        for (ObjectType type : types)
        {
            builder.append(type.toString());
            if (type != last)
                builder.append("/");
        }

        return builder.toString();
    }
}
