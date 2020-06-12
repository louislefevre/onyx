package compilation.analysis.semantic;

import lombok.Getter;
import types.ObjectType;
import types.OperatorType;
import types.TokenType;

/**
 * The AnnotatedBinaryOperator class is used to store information about annotated binary operators declared during
 * compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedBinaryOperator implements AnnotatedOperator
{
    private final TokenType tokenType;
    private final OperatorType operatorType;
    private final ObjectType leftObjectType;
    private final ObjectType rightObjectType;
    private final ObjectType resultObjectType;

    /**
     * Constructs an AnnotatedBinaryOperator object, initialised with the types of the expressions contents.
     *
     * @param tokenType The operator TokenType
     * @param operatorType The operator OperatorType
     * @param leftObjectType The left operand ObjectType
     * @param rightObjectType The right operand ObjectType
     * @param resultObjectType The result ObjectType
     */
    public AnnotatedBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType leftObjectType,
                                   ObjectType rightObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.leftObjectType = leftObjectType;
        this.rightObjectType = rightObjectType;
        this.resultObjectType = resultObjectType;
    }

    /**
     * Constructs an AnnotatedBinaryOperator object, initialised with the types of the expressions contents.
     * <p>
     * The leftObjectType and rightObjectType fields are assigned the value of the operandObjectType parameter.
     *
     * @param tokenType The operator TokenType
     * @param operatorType The operator OperatorType
     * @param operandObjectType The left and right operand ObjectType
     * @param resultObjectType The result ObjectType
     */
    public AnnotatedBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType operandObjectType,
                                   ObjectType resultObjectType)
    {
        this(tokenType, operatorType, operandObjectType, operandObjectType, resultObjectType);
    }

    /**
     * Constructs an AnnotatedBinaryOperator object, initialised with the types of the expressions contents.
     * <p>
     * The leftObjectType, rightObjectType, and resultObjectType fields are assigned the value of the objectType
     * parameter.
     *
     * @param tokenType The operator TokenType
     * @param operatorType The operator OperatorType
     * @param objectType The left operand, right operand, and result ObjectType
     */
    public AnnotatedBinaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType objectType)
    {
        this(tokenType, operatorType, objectType, objectType, objectType);
    }
}
