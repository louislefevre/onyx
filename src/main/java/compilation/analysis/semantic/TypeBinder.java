package compilation.analysis.semantic;

import org.jetbrains.annotations.Nullable;
import types.ObjectType;
import types.TokenType;

import static types.ObjectType.*;
import static types.OperatorType.*;
import static types.TokenType.*;

/**
 * The TypeBinder class is used to identify if an operator and operand(s) are compatible with one another.
 * <p>
 * Using pre-defined operator combinations, the class takes an operator and operand(s) and compares them against the
 * corresponding pre-defined set. For example, to check whether an addition operator is valid to use between an integer
 * and double. If true, an AnnotatedOperator object is returned. If false, null is returned.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class TypeBinder
{
    private static final AnnotatedUnaryOperator[] unaryOperators =
    {
        // Integer Objects
        new AnnotatedUnaryOperator(PLUS_TOKEN, POSITIVE_OPERATOR, INTEGER_OBJECT),
        new AnnotatedUnaryOperator(MINUS_TOKEN, NEGATIVE_OPERATOR, INTEGER_OBJECT),

        // Double Objects
        new AnnotatedUnaryOperator(PLUS_TOKEN, POSITIVE_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedUnaryOperator(MINUS_TOKEN, NEGATIVE_OPERATOR, DOUBLE_OBJECT),

        // Boolean Objects
        new AnnotatedUnaryOperator(NOT_TOKEN, NEGATION_OPERATOR, BOOLEAN_OBJECT)
    };

    private static final AnnotatedBinaryOperator[] binaryOperators =
    {
        // Integer Objects
        new AnnotatedBinaryOperator(PLUS_TOKEN, ADDITION_OPERATOR, INTEGER_OBJECT),
        new AnnotatedBinaryOperator(MINUS_TOKEN, SUBTRACTION_OPERATOR, INTEGER_OBJECT),
        new AnnotatedBinaryOperator(STAR_TOKEN, MULTIPLICATION_OPERATOR, INTEGER_OBJECT),
        new AnnotatedBinaryOperator(SLASH_TOKEN, DIVISION_OPERATOR, INTEGER_OBJECT),
        new AnnotatedBinaryOperator(PERCENT_TOKEN, MODULO_OPERATOR, INTEGER_OBJECT),
        new AnnotatedBinaryOperator(CARET_TOKEN, POWER_OPERATOR, INTEGER_OBJECT),
        new AnnotatedBinaryOperator(GREATER_TOKEN, GREATER_OPERATOR, INTEGER_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(LESS_TOKEN, LESS_OPERATOR, INTEGER_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(GREATER_EQUALS_TOKEN, GREATER_EQUALS_OPERATOR, INTEGER_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(LESS_EQUALS_TOKEN, LESS_EQUALS_OPERATOR, INTEGER_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(EQUALS_EQUALS_TOKEN, EQUALS_EQUALS_OPERATOR, INTEGER_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(NOT_EQUALS_TOKEN, NOT_EQUALS_OPERATOR, INTEGER_OBJECT, BOOLEAN_OBJECT),

        // Double Objects
        new AnnotatedBinaryOperator(PLUS_TOKEN, ADDITION_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedBinaryOperator(MINUS_TOKEN, SUBTRACTION_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedBinaryOperator(STAR_TOKEN, MULTIPLICATION_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedBinaryOperator(SLASH_TOKEN, DIVISION_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedBinaryOperator(PERCENT_TOKEN, MODULO_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedBinaryOperator(CARET_TOKEN, POWER_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedBinaryOperator(GREATER_TOKEN, GREATER_OPERATOR, DOUBLE_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(LESS_TOKEN, LESS_OPERATOR, DOUBLE_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(GREATER_EQUALS_TOKEN, GREATER_EQUALS_OPERATOR, DOUBLE_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(LESS_EQUALS_TOKEN, LESS_EQUALS_OPERATOR, DOUBLE_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(EQUALS_EQUALS_TOKEN, EQUALS_EQUALS_OPERATOR, DOUBLE_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(NOT_EQUALS_TOKEN, NOT_EQUALS_OPERATOR, DOUBLE_OBJECT, BOOLEAN_OBJECT),

        // Boolean Objects
        new AnnotatedBinaryOperator(EQUALS_EQUALS_TOKEN, EQUALS_EQUALS_OPERATOR, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(NOT_EQUALS_TOKEN, NOT_EQUALS_OPERATOR, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(AND_TOKEN, AND_OPERATOR, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(OR_TOKEN, OR_OPERATOR, BOOLEAN_OBJECT),

        // String Objects
        new AnnotatedBinaryOperator(PLUS_TOKEN, ADDITION_OPERATOR, STRING_OBJECT),
        new AnnotatedBinaryOperator(EQUALS_EQUALS_TOKEN, EQUALS_EQUALS_OPERATOR, STRING_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedBinaryOperator(NOT_EQUALS_TOKEN, NOT_EQUALS_OPERATOR, STRING_OBJECT, BOOLEAN_OBJECT)
    };

    private static final AnnotatedAssignmentOperator[] assignmentOperators =
    {
        // Integer Objects
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, INTEGER_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, INTEGER_OBJECT, DOUBLE_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, INTEGER_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, INTEGER_OBJECT, STRING_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, NULL_OBJECT, INTEGER_OBJECT),
        new AnnotatedAssignmentOperator(PLUS_EQUALS_TOKEN, ADDITION_OPERATOR, INTEGER_OBJECT),
        new AnnotatedAssignmentOperator(MINUS_EQUALS_TOKEN, SUBTRACTION_OPERATOR, INTEGER_OBJECT),
        new AnnotatedAssignmentOperator(STAR_EQUALS_TOKEN, MULTIPLICATION_OPERATOR, INTEGER_OBJECT),
        new AnnotatedAssignmentOperator(SLASH_EQUALS_TOKEN, DIVISION_OPERATOR, INTEGER_OBJECT),
        new AnnotatedAssignmentOperator(PERCENT_EQUALS_TOKEN, MODULO_OPERATOR, INTEGER_OBJECT),
        new AnnotatedAssignmentOperator(CARET_EQUALS_TOKEN, POWER_OPERATOR, INTEGER_OBJECT),

        // Double Objects
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, DOUBLE_OBJECT, INTEGER_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, DOUBLE_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, DOUBLE_OBJECT, STRING_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, NULL_OBJECT, DOUBLE_OBJECT),
        new AnnotatedAssignmentOperator(PLUS_EQUALS_TOKEN, ADDITION_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedAssignmentOperator(MINUS_EQUALS_TOKEN, SUBTRACTION_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedAssignmentOperator(STAR_EQUALS_TOKEN, MULTIPLICATION_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedAssignmentOperator(SLASH_EQUALS_TOKEN, DIVISION_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedAssignmentOperator(PERCENT_EQUALS_TOKEN, MODULO_OPERATOR, DOUBLE_OBJECT),
        new AnnotatedAssignmentOperator(CARET_EQUALS_TOKEN, POWER_OPERATOR, DOUBLE_OBJECT),

        // Boolean Objects
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, BOOLEAN_OBJECT, INTEGER_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, BOOLEAN_OBJECT, DOUBLE_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, BOOLEAN_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, BOOLEAN_OBJECT, STRING_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, NULL_OBJECT, BOOLEAN_OBJECT),

        // String Objects
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, STRING_OBJECT, INTEGER_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, STRING_OBJECT, DOUBLE_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, STRING_OBJECT, BOOLEAN_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, STRING_OBJECT),
        new AnnotatedAssignmentOperator(EQUALS_TOKEN, EQUALS_OPERATOR, NULL_OBJECT, STRING_OBJECT),
        new AnnotatedAssignmentOperator(PLUS_EQUALS_TOKEN, ADDITION_OPERATOR, STRING_OBJECT)
    };

    /**
     * Attempts to bind an operator to an operand, returning an AnnotatedUnaryOperator object if they are compatible.
     * <p>
     * If the operator and operand are not compatible, null is returned.
     *
     * @param operatorType The TokenType of the operator
     * @param operandType The ObjectType of the operand
     * @return An AnnotatedUnaryOperator object containing the expressions type enums
     */
    @Nullable
    public static AnnotatedUnaryOperator bindUnaryOperators(TokenType operatorType, ObjectType operandType)
    {
        for (AnnotatedUnaryOperator operator : unaryOperators)
            if (operator.getTokenType() == operatorType &&
                operator.getOperandObjectType() == operandType)
                return operator;

        return null;
    }

    /**
     * Attempts to bind a two operands with an operator, returning an AnnotatedBinaryOperator object if they are
     * compatible.
     * <p>
     * If the operator and operands are not compatible, null is returned.
     *
     * @param operatorType The TokenType of the operator
     * @param leftOperandType The ObjectType of the left operand
     * @param rightOperandType The ObjectType of the right operand
     * @return An AnnotatedBinaryOperator object containing the expressions type enums
     */
    @Nullable
    public static AnnotatedBinaryOperator bindBinaryOperators(TokenType operatorType, ObjectType leftOperandType,
                                                              ObjectType rightOperandType)
    {
        for (AnnotatedBinaryOperator operator : binaryOperators)
            if (operator.getTokenType() == operatorType &&
                operator.getLeftObjectType() == leftOperandType &&
                operator.getRightObjectType() == rightOperandType)
                return operator;

        return null;
    }

    /**
     * Attempts to bind an assignment operator to a symbol and assignment, returning an AnnotatedAssignmentOperator
     * object if they are compatible.
     * <p>
     * If the operator, symbol, and assignment are not compatible, null is returned.
     *
     * @param operatorType The TokenType of the operator
     * @param symbolType The ObjectType of the symbol
     * @param assignmentType The ObjectType of the assignment
     * @return An AnnotatedAssignmentOperator object containing the expressions type enums
     */
    @Nullable
    public static AnnotatedAssignmentOperator bindAssignmentOperators(TokenType operatorType, ObjectType symbolType,
                                                                      ObjectType assignmentType)
    {
        for (AnnotatedAssignmentOperator operator : assignmentOperators)
            if (operator.getTokenType() == operatorType &&
                operator.getIdentifierObjectType() == symbolType &&
                operator.getAssignmentObjectType() == assignmentType)
                return operator;

        return null;
    }
}
