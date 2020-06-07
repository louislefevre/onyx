package errors;

import source.SourceSpan;
import types.ObjectType;

import static types.ErrorType.SEMANTIC_ERROR;

public final class SemanticError extends Error
{
    public SemanticError(SourceSpan span, String problem, String solution)
    {
        super(SEMANTIC_ERROR, span, problem, solution);
    }

    public static SemanticError undefinedUnaryOperator(SourceSpan span, String syntax, ObjectType type)
    {
        String problem = String.format("The operator '%s' cannot be used with a %s.", syntax, type.toString());
        String solution = "Remove the operator or change it to a valid one.";

        return new SemanticError(span, problem, solution);
    }

    public static SemanticError undefinedBinaryOperator(SourceSpan span, String syntax, ObjectType leftType,
                                                        ObjectType rightType)
    {
        String problem = String.format("You cannot use the operator '%s' here, as the types of the values do not match." +
                                       System.lineSeparator() +
                                       "The left value is a %s, while the right value is a %s.", syntax, leftType.toString(), rightType.toString());
        String solution = String.format("Change one of the values so that they are of the same type (e.g. change them both to a %s or %s).", leftType.toString(), rightType.toString());

        return new SemanticError(span, problem, solution);
    }

    public static SemanticError undefinedAssignmentOperator(SourceSpan span, String syntax, ObjectType symbolType,
                                                            ObjectType assignmentType)
    {
        String problem = String.format("You cannot use the operator '%s' here, as the variable type does not match the assignment type." +
                                       System.lineSeparator() +
                                       "The variable contains a %s, but you are trying to assign a %s.", syntax, symbolType.toString(), assignmentType.toString());
        String solution = String.format("Reassign the variable to change its type to a %s.", symbolType.toString());

        return new SemanticError(span, problem, solution);
    }

    public static SemanticError undefinedIdentifier(SourceSpan span, String syntax)
    {
        String problem = String.format("The variable '%s' does not exist. This may be because you are creating it inside a false if statement.", syntax);
        String solution = "Create the variable by assigning a value to it (e.g. \"var = 0\"). It may help to write this outside of any if statements.";

        return new SemanticError(span, problem, solution);
    }

    public static SemanticError invalidExpressionTypes(SourceSpan span, ObjectType actualType, ObjectType[] targetTypes)
    {
        String problem = String.format("This expression is a %s type, and cannot be used here.", actualType.toString());
        String solution = String.format("Change the expression so that it instead uses a %s.", typesToString(targetTypes));

        return new SemanticError(span, problem, solution);
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
