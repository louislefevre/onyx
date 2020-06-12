package compilation.analysis.semantic;

import lombok.Getter;
import types.ObjectType;
import types.OperatorType;
import types.TokenType;

/**
 * The AnnotatedUnaryOperator class is used to store information about annotated unary operators declared during
 * compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedUnaryOperator implements AnnotatedOperator
{
    private final TokenType tokenType;
    private final OperatorType operatorType;
    private final ObjectType operandObjectType;
    private final ObjectType resultObjectType;

    /**
     * Constructs an AnnotatedUnaryOperator object, initialised with the types of the expressions contents.
     *
     * @param tokenType The operator TokenType
     * @param operatorType The operator OperatorType
     * @param operandObjectType The operand ObjectType
     * @param resultObjectType The result ObjectType
     */
    public AnnotatedUnaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType operandObjectType,
                                  ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.operandObjectType = operandObjectType;
        this.resultObjectType = resultObjectType;
    }

    /**
     * Constructs an AnnotatedUnaryOperator object, initialised with the types of the expressions contents.
     * <p>
     * The operandObjectType and resultObjectType fields are assigned the value of the objectType parameter.
     *
     * @param tokenType The operator TokenType
     * @param operatorType The operator OperatorType
     * @param objectType The operand and result ObjectType
     */
    public AnnotatedUnaryOperator(TokenType tokenType, OperatorType operatorType, ObjectType objectType)
    {
        this(tokenType, operatorType, objectType, objectType);
    }
}
