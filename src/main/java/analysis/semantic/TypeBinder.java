package analysis.semantic;

import identifiers.ObjectType;
import identifiers.OperatorType;
import identifiers.TokenType;
import org.jetbrains.annotations.Nullable;

public final class TypeBinder
{
    private TypeBinder()
    {
        throw new UnsupportedOperationException();
    }

    private static final AnnotatedUnaryOperator[] unaryOperators =
            {
                    // Integer Objects
                    new AnnotatedUnaryOperator(TokenType.PLUS_TOKEN,
                                               OperatorType.IDENTITY_OPERATOR,
                                               ObjectType.INTEGER_OBJECT),
                    new AnnotatedUnaryOperator(TokenType.MINUS_TOKEN,
                                               OperatorType.NEGATION_OPERATOR,
                                               ObjectType.INTEGER_OBJECT),

                    // Double Objects
                    new AnnotatedUnaryOperator(TokenType.PLUS_TOKEN,
                                               OperatorType.IDENTITY_OPERATOR,
                                               ObjectType.DOUBLE_OBJECT),
                    new AnnotatedUnaryOperator(TokenType.MINUS_TOKEN,
                                               OperatorType.NEGATION_OPERATOR,
                                               ObjectType.DOUBLE_OBJECT),

                    // Boolean Objects
                    new AnnotatedUnaryOperator(TokenType.NOT_TOKEN,
                                               OperatorType.LOGIC_NEGATION_OPERATOR,
                                               ObjectType.BOOLEAN_OBJECT),
                    };

    private static final AnnotatedBinaryOperator[] binaryOperators =
            {
                    // Integer Objects
                    new AnnotatedBinaryOperator(TokenType.PLUS_TOKEN,
                                                OperatorType.ADDITION_OPERATOR,
                                                ObjectType.INTEGER_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.MINUS_TOKEN,
                                                OperatorType.SUBTRACTION_OPERATOR,
                                                ObjectType.INTEGER_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.STAR_TOKEN,
                                                OperatorType.MULTIPLICATION_OPERATOR,
                                                ObjectType.INTEGER_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.SLASH_TOKEN,
                                                OperatorType.DIVISION_OPERATOR,
                                                ObjectType.INTEGER_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.CARET_TOKEN,
                                                OperatorType.POWER_OPERATOR,
                                                ObjectType.INTEGER_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.PERCENT_TOKEN,
                                                OperatorType.MODULO_OPERATOR,
                                                ObjectType.INTEGER_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.GREATER_TOKEN,
                                                OperatorType.GREATER_OPERATOR,
                                                ObjectType.INTEGER_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.LESS_TOKEN,
                                                OperatorType.LESS_OPERATOR,
                                                ObjectType.INTEGER_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.GREATER_EQUALS_TOKEN,
                                                OperatorType.GREATER_EQUALS_OPERATOR,
                                                ObjectType.INTEGER_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.LESS_EQUALS_TOKEN,
                                                OperatorType.LESS_EQUALS_OPERATOR,
                                                ObjectType.INTEGER_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.EQUALS_EQUALS_TOKEN,
                                                OperatorType.EQUALS_EQUALS_OPERATOR,
                                                ObjectType.INTEGER_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.NOT_EQUALS_TOKEN,
                                                OperatorType.NOT_EQUALS_OPERATOR,
                                                ObjectType.INTEGER_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),

                    // Double Objects
                    new AnnotatedBinaryOperator(TokenType.PLUS_TOKEN,
                                                OperatorType.ADDITION_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.MINUS_TOKEN,
                                                OperatorType.SUBTRACTION_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.STAR_TOKEN,
                                                OperatorType.MULTIPLICATION_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.SLASH_TOKEN,
                                                OperatorType.DIVISION_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.CARET_TOKEN,
                                                OperatorType.POWER_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.PERCENT_TOKEN,
                                                OperatorType.MODULO_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.GREATER_TOKEN,
                                                OperatorType.GREATER_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.LESS_TOKEN,
                                                OperatorType.LESS_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.GREATER_EQUALS_TOKEN,
                                                OperatorType.GREATER_EQUALS_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.LESS_EQUALS_TOKEN,
                                                OperatorType.LESS_EQUALS_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.EQUALS_EQUALS_TOKEN,
                                                OperatorType.EQUALS_EQUALS_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.NOT_EQUALS_TOKEN,
                                                OperatorType.NOT_EQUALS_OPERATOR,
                                                ObjectType.DOUBLE_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),

                    // Boolean Objects
                    new AnnotatedBinaryOperator(TokenType.EQUALS_EQUALS_TOKEN,
                                                OperatorType.EQUALS_EQUALS_OPERATOR,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.NOT_EQUALS_TOKEN,
                                                OperatorType.NOT_EQUALS_OPERATOR,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.AND_TOKEN,
                                                OperatorType.AND_OPERATOR,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.OR_TOKEN,
                                                OperatorType.OR_OPERATOR,
                                                ObjectType.BOOLEAN_OBJECT),

                    // String Objects
                    new AnnotatedBinaryOperator(TokenType.PLUS_TOKEN,
                                                OperatorType.ADDITION_OPERATOR,
                                                ObjectType.STRING_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.EQUALS_EQUALS_TOKEN,
                                                OperatorType.EQUALS_EQUALS_OPERATOR,
                                                ObjectType.STRING_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT),
                    new AnnotatedBinaryOperator(TokenType.NOT_EQUALS_TOKEN,
                                                OperatorType.NOT_EQUALS_OPERATOR,
                                                ObjectType.STRING_OBJECT,
                                                ObjectType.BOOLEAN_OBJECT)
            };

    @Nullable
    public static AnnotatedUnaryOperator bindUnaryOperators(TokenType tokenType, ObjectType operandObjectType)
    {
        for (AnnotatedUnaryOperator operator : unaryOperators)
            if (operator.getTokenType() == tokenType && operator.getOperandObjectType() == operandObjectType)
                return operator;

        return null;
    }

    @Nullable
    public static AnnotatedBinaryOperator bindBinaryOperators(TokenType tokenType, ObjectType leftObjectType,
                                                              ObjectType rightObjectType)
    {
        for (AnnotatedBinaryOperator operator : binaryOperators)
            if (operator.getTokenType() == tokenType &&
                operator.getLeftObjectType() == leftObjectType &&
                operator.getRightObjectType() == rightObjectType)
                return operator;

        return null;
    }
}
