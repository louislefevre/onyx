package analysis.semantic;

import lombok.Getter;
import types.ObjectType;
import types.OperatorType;
import types.TokenType;

@Getter
public final class AnnotatedAssignmentOperator implements AnnotatedOperator
{
    private final TokenType tokenType;
    private final OperatorType operatorType;
    private final ObjectType identifierObjectType;
    private final ObjectType assignmentObjectType;
    private final ObjectType resultObjectType;

    public AnnotatedAssignmentOperator(TokenType tokenType, OperatorType operatorType, ObjectType identifierObjectType,
                                       ObjectType assignmentObjectType, ObjectType resultObjectType)
    {
        this.tokenType = tokenType;
        this.operatorType = operatorType;
        this.identifierObjectType = identifierObjectType;
        this.assignmentObjectType = assignmentObjectType;
        this.resultObjectType = resultObjectType;
    }

    public AnnotatedAssignmentOperator(TokenType tokenType, OperatorType operatorType, ObjectType identifierObjectType,
                                       ObjectType objectType)
    {
        this(tokenType, operatorType, identifierObjectType, objectType, objectType);
    }

    public AnnotatedAssignmentOperator(TokenType tokenType, OperatorType operatorType, ObjectType objectType)
    {
        this(tokenType, operatorType, objectType, objectType, objectType);
    }
}
