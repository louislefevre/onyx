package analysis.semantic;

import identifiers.ObjectType;
import identifiers.OperatorType;
import identifiers.TokenType;

public final class TypeBinder
{
    private TypeBinder() { throw new UnsupportedOperationException(); }

    private static final BoundUnaryOperator[] unaryOperators =
    {
        new BoundUnaryOperator(TokenType.NOT_TOKEN, OperatorType.LOGIC_NEGATION_OPERATOR, ObjectType.BOOLEAN_OBJECT),
        new BoundUnaryOperator(TokenType.PLUS_TOKEN, OperatorType.IDENTITY_OPERATOR, ObjectType.INTEGER_OBJECT),
        new BoundUnaryOperator(TokenType.MINUS_TOKEN, OperatorType.NEGATION_OPERATOR, ObjectType.INTEGER_OBJECT)
    };

    private static final BoundBinaryOperator[] binaryOperators =
    {
        new BoundBinaryOperator(TokenType.PLUS_TOKEN, OperatorType.ADDITION_OPERATOR, ObjectType.INTEGER_OBJECT),
        new BoundBinaryOperator(TokenType.MINUS_TOKEN, OperatorType.SUBTRACTION_OPERATOR, ObjectType.INTEGER_OBJECT),
        new BoundBinaryOperator(TokenType.STAR_TOKEN, OperatorType.MULTIPLICATION_OPERATOR, ObjectType.INTEGER_OBJECT),
        new BoundBinaryOperator(TokenType.SLASH_TOKEN, OperatorType.DIVISION_OPERATOR, ObjectType.INTEGER_OBJECT),
        new BoundBinaryOperator(TokenType.CARET_TOKEN, OperatorType.POWER_OPERATOR, ObjectType.INTEGER_OBJECT),
        new BoundBinaryOperator(TokenType.PERCENT_TOKEN, OperatorType.MODULO_OPERATOR, ObjectType.INTEGER_OBJECT),

        new BoundBinaryOperator(TokenType.EQUALS_EQUALS_TOKEN, OperatorType.EQUALS_EQUALS_OPERATOR, ObjectType.BOOLEAN_OBJECT),
        new BoundBinaryOperator(TokenType.NOT_EQUALS_TOKEN, OperatorType.NOT_EQUALS_OPERATOR, ObjectType.BOOLEAN_OBJECT),
        new BoundBinaryOperator(TokenType.AND_TOKEN, OperatorType.AND_OPERATOR, ObjectType.BOOLEAN_OBJECT),
        new BoundBinaryOperator(TokenType.OR_TOKEN, OperatorType.OR_OPERATOR, ObjectType.BOOLEAN_OBJECT),

        new BoundBinaryOperator(TokenType.GREATER_TOKEN, OperatorType.GREATER_OPERATOR, ObjectType.INTEGER_OBJECT, ObjectType.BOOLEAN_OBJECT),
        new BoundBinaryOperator(TokenType.LESS_TOKEN, OperatorType.LESS_OPERATOR, ObjectType.INTEGER_OBJECT, ObjectType.BOOLEAN_OBJECT),
        new BoundBinaryOperator(TokenType.GREATER_EQUALS_TOKEN, OperatorType.GREATER_EQUALS_OPERATOR, ObjectType.INTEGER_OBJECT, ObjectType.BOOLEAN_OBJECT),
        new BoundBinaryOperator(TokenType.LESS_EQUALS_TOKEN, OperatorType.LESS_EQUALS_OPERATOR, ObjectType.INTEGER_OBJECT, ObjectType.BOOLEAN_OBJECT),

        new BoundBinaryOperator(TokenType.EQUALS_EQUALS_TOKEN, OperatorType.EQUALS_EQUALS_OPERATOR, ObjectType.INTEGER_OBJECT, ObjectType.BOOLEAN_OBJECT),
        new BoundBinaryOperator(TokenType.NOT_EQUALS_TOKEN, OperatorType.NOT_EQUALS_OPERATOR, ObjectType.INTEGER_OBJECT, ObjectType.BOOLEAN_OBJECT)
    };

    public static BoundUnaryOperator bindUnaryOperators(TokenType tokenType, ObjectType operandObjectType)
    {
        for(BoundUnaryOperator operator : unaryOperators)
        {
            if(operator.getTokenType() == tokenType && operator.getOperandObjectType() == operandObjectType)
                return operator;
        }

        return null;
    }

    public static BoundBinaryOperator bindBinaryOperators(TokenType tokenType, ObjectType leftObjectType, ObjectType rightObjectType)
    {
        for(BoundBinaryOperator operator : binaryOperators)
        {
            if(operator.getTokenType() == tokenType && operator.getLeftObjectType() == leftObjectType && operator.getRightObjectType() == rightObjectType)
                return operator;
        }

        return null;
    }
}
