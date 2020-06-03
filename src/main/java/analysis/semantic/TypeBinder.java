package analysis.semantic;

import org.jetbrains.annotations.Nullable;
import types.ObjectType;
import types.TokenType;

import static types.ObjectType.*;
import static types.OperatorType.*;
import static types.TokenType.*;

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

    @Nullable
    public static AnnotatedUnaryOperator bindUnaryOperators(TokenType operatorType, ObjectType operandType)
    {
        for (AnnotatedUnaryOperator operator : unaryOperators)
            if (operator.getTokenType() == operatorType &&
                operator.getOperandObjectType() == operandType)
                return operator;

        return null;
    }

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
