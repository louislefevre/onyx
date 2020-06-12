package exceptions;

import types.AnnotatedExpressionType;
import types.AnnotatedStatementType;
import types.ObjectType;
import types.OperatorType;

import static types.ExceptionType.EVALUATION_EXCEPTION;

public final class EvaluationException extends Exception
{
    public EvaluationException(String message)
    {
        super(EVALUATION_EXCEPTION, message);
    }

    /**
     * Generate and return a String message for an unexpected annotated expression exception.
     *
     * @param type The type of the expression
     * @return A String containing the exception message
     */
    public static String unexpectedAnnotatedExpression(AnnotatedExpressionType type)
    {
        return String.format("Unexpected annotated expression '%s'.", type.toString());
    }

    /**
     * Generate and return a String message for an unexpected annotated statement exception.
     *
     * @param type The type of the statement
     * @return A String containing the exception message
     */
    public static String unexpectedAnnotatedStatement(AnnotatedStatementType type)
    {
        return String.format("Unexpected annotated statement '%s'.", type.toString());
    }

    /**
     * Generate and return a String message for an unexpected unary object type exception.
     *
     * @param type The type of the unary operand
     * @return A String containing the exception message
     */
    public static String unexpectedUnaryObjectType(ObjectType type)
    {
        return String.format("Unexpected unary object type '%s'.", type.toString());
    }

    /**
     * Generate and return a String message for an unexpected binary object type exception.
     *
     * @param leftType The type of the left operand
     * @param rightType The type of the right operand
     * @return A String containing the exception message
     */
    public static String unexpectedBinaryObjectTypes(ObjectType leftType, ObjectType rightType)
    {
        return String.format("Unexpected binary object types '%1s' and '%2s'.", leftType.toString(), rightType.toString());
    }

    /**
     * Generate and return a String message for an unexpected assignment object type exception.
     *
     * @param symbolType The type of the symbol
     * @param assignmentType The type of the assignment
     * @return A String containing the exception message
     */
    public static String unexpectedAssignmentObjectTypes(ObjectType symbolType, ObjectType assignmentType)
    {
        return String.format("Unexpected assignment object types '%1s' and '%2s'.", symbolType.toString(), assignmentType.toString());
    }

    /**
     * Generate and return a String message for an unexpected unary operator exception.
     *
     * @param type The type of the unary operator
     * @return A String containing the exception message
     */
    public static String unexpectedUnaryOperator(OperatorType type)
    {
        return String.format("Unexpected unary operator '%s'.", type.toString());
    }

    /**
     * Generate and return a String message for an unexpected binary operator exception.
     *
     * @param type The type of the binary operator
     * @return A String containing the exception message
     */
    public static String unexpectedBinaryOperator(OperatorType type)
    {
        return String.format("Unexpected binary operator '%s'.", type.toString());
    }

    /**
     * Generate and return a String message for an unexpected assignment operator exception.
     *
     * @param type The type of the assignment operator
     * @return A String containing the exception message
     */
    public static String unexpectedAssignmentOperator(OperatorType type)
    {
        return String.format("Unexpected assignment operator '%s'.", type.toString());
    }

    /**
     * Generate and return a String message for a missing symbol exception.
     *
     * @param name The name of the missing symbol
     * @return A String containing the exception message
     */
    public static String missingSymbol(String name)
    {
        return String.format("Symbol '%s' does not exist in symbol table.", name);
    }

    /**
     * Generate and return a String message for an invalid loop types exception.
     *
     * @param lowerType The type of the lower bound
     * @param upperType The type of the upper bound
     * @return A String containing the exception message
     */
    public static String invalidLoopTypes(ObjectType lowerType, ObjectType upperType)
    {
        return String.format("Invalid loop object types '%1s' and '%2s'.", lowerType.toString(), upperType.toString());
    }

    /**
     * Generate and return a String message for an invalid conditional types exception.
     *
     * @param type The type of the condition
     * @return A String containing the exception message
     */
    public static String invalidConditionalType(ObjectType type)
    {
        return String.format("Invalid conditional object type '%s'.", type.toString());
    }
}
