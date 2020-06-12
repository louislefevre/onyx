package errors;

import source.SourceSpan;
import types.ObjectType;

import static types.ErrorType.SEMANTIC_ERROR;

/**
 * The SemanticError class is used to represent an error that occurred during the semantic analysis stage of compilation.
 * <p>
 * Static methods for generating SemanticErrors are stored here, each of which uses a set of pre-defined input Strings.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class SemanticError extends Error
{
    public SemanticError(SourceSpan span, String problem, String solution)
    {
        super(SEMANTIC_ERROR, span, problem, solution);
    }

    /**
     * Generate and return a SemanticError for an incompatible unary operator.
     *
     * @param span The location information for where an error occurred
     * @param syntax The syntax of the operator
     * @param type The ObjectType of the operand
     * @return A SemanticError containing information about an error that occurred
     */
    public static SemanticError undefinedUnaryOperator(SourceSpan span, String syntax, ObjectType type)
    {
        String problem = String.format("The operator '%s' cannot be used with a %s.", syntax, type.toString());
        String solution = "Remove the operator or change it to a valid one.";

        return new SemanticError(span, problem, solution);
    }

    /**
     * Generate and return a SemanticError for an incompatible binary operator.
     *
     * @param span The location information for where an error occurred
     * @param syntax The syntax of the operator
     * @param leftType The ObjectType of the left operand
     * @param rightType The ObjectType of the right operand
     * @return A SemanticError containing information about an error that occurred
     */
    public static SemanticError undefinedBinaryOperator(SourceSpan span, String syntax, ObjectType leftType,
                                                        ObjectType rightType)
    {
        String problem = String.format("You cannot use the operator '%s' here, as the types of the values do not match." +
                                       System.lineSeparator() +
                                       "The left value is a %s, while the right value is a %s.", syntax, leftType.toString(), rightType.toString());
        String solution = String.format("Change one of the values so that they are of the same type (e.g. change them both to a %s or %s).", leftType.toString(), rightType.toString());

        return new SemanticError(span, problem, solution);
    }

    /**
     * Generate and return a SemanticError for an incompatible assignment operator.
     *
     * @param span The location information for where an error occurred
     * @param syntax The syntax of the operator
     * @param symbolType The ObjectType of the symbol
     * @param assignmentType The ObjectType of the assignment
     * @return A SemanticError containing information about an error that occurred
     */
    public static SemanticError undefinedAssignmentOperator(SourceSpan span, String syntax, ObjectType symbolType,
                                                            ObjectType assignmentType)
    {
        String problem = String.format("You cannot use the operator '%s' here, as the variable type does not match the assignment type." +
                                       System.lineSeparator() +
                                       "The variable contains a %s, but you are trying to assign a %s.", syntax, symbolType.toString(), assignmentType.toString());
        String solution = String.format("Reassign the variable to change its type to a %s.", symbolType.toString());

        return new SemanticError(span, problem, solution);
    }

    /**
     * Generate and return a SemanticError for an undeclared symbol.
     *
     * @param span The location information for where an error occurred
     * @param syntax The syntax of the identifier
     * @return A SemanticError containing information about an error that occurred
     */
    public static SemanticError undefinedIdentifier(SourceSpan span, String syntax)
    {
        String problem = String.format("The variable '%s' does not exist. This may be because you are creating it inside a false if statement.", syntax);
        String solution = "Create the variable by assigning a value to it (e.g. \"var = 0\"). It may help to write this outside of any if statements.";

        return new SemanticError(span, problem, solution);
    }

    /**
     * Generate and return a SemanticError for invalid expression type(s).
     *
     * @param span The location information for where an error occurred
     * @param actualType The actual ObjectType of the expression
     * @param targetTypes The expected ObjectType of the expression
     * @return A SemanticError containing information about an error that occurred
     */
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
