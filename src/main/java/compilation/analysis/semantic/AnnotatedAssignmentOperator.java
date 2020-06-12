package compilation.analysis.semantic;

import lombok.Getter;
import types.ObjectType;
import types.OperatorType;
import types.TokenType;

/**
 * The AnnotatedAssignmentOperator class is used to store information about annotated assignment operators declared
 * during compilation.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
public final class AnnotatedAssignmentOperator implements AnnotatedOperator
{
    private final TokenType tokenType;
    private final OperatorType operatorType;
    private final ObjectType identifierObjectType;
    private final ObjectType assignmentObjectType;
    private final ObjectType resultObjectType;

    /**
     * Constructs an AnnotatedAssignmentOperator object, initialised with the types of the expressions contents.
     *
     * @param tokenType The operator TokenType
     * @param operatorType The operator OperatorType
     * @param identifierObjectType The identifier ObjectType
     * @param assignmentObjectType The assignment ObjectType
     * @param resultObjectType The result ObjectType
     */
    public AnnotatedAssignmentOperator(TokenType tokenType, OperatorType operatorType, ObjectType identifierObjectType,
                                       ObjectType assignmentObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.identifierObjectType = identifierObjectType;
        this.assignmentObjectType = assignmentObjectType;
        this.resultObjectType = resultObjectType;
    }

    /**
     * Constructs an AnnotatedAssignmentOperator object, initialised with the types of the expressions contents.
     * <p>
     * The assignmentObjectType and resultObjectType fields are assigned the value of the objectType parameter.
     *
     * @param tokenType The operator TokenType
     * @param operatorType The operator OperatorType
     * @param identifierObjectType The identifier ObjectType
     * @param objectType The assignment and result ObjectType
     */
    public AnnotatedAssignmentOperator(TokenType tokenType, OperatorType operatorType, ObjectType identifierObjectType,
                                       ObjectType objectType)
    {
        this(tokenType, operatorType, identifierObjectType, objectType, objectType);
    }

    /**
     * Constructs an AnnotatedAssignmentOperator object, initialised with the types of the expressions contents.
     * <p>
     * The identifierObjectType, assignmentObjectType, and resultObjectType fields are assigned the value of the
     * objectType parameter.
     *
     * @param tokenType The operator TokenType
     * @param operatorType The operator OperatorType
     * @param objectType The identifier, assignment, and result ObjectType
     */
    public AnnotatedAssignmentOperator(TokenType tokenType, OperatorType operatorType, ObjectType objectType)
    {
        this(tokenType, operatorType, objectType, objectType, objectType);
    }
}
