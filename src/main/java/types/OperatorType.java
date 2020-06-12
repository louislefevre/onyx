package types;

/**
 * The OperatorType enum is used to store constants related to AnnotatedOperator objects.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public enum OperatorType
{
    // Unary Operators
    POSITIVE_OPERATOR,
    NEGATIVE_OPERATOR,
    NEGATION_OPERATOR,

    // Binary Operators
    ADDITION_OPERATOR,
    SUBTRACTION_OPERATOR,
    MULTIPLICATION_OPERATOR,
    DIVISION_OPERATOR,
    MODULO_OPERATOR,
    POWER_OPERATOR,
    GREATER_OPERATOR,
    GREATER_EQUALS_OPERATOR,
    LESS_OPERATOR,
    LESS_EQUALS_OPERATOR,
    EQUALS_OPERATOR,
    EQUALS_EQUALS_OPERATOR,
    NOT_EQUALS_OPERATOR,
    AND_OPERATOR,
    OR_OPERATOR
}
