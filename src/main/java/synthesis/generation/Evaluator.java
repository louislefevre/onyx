package synthesis.generation;

import analysis.semantic.*;
import errors.ErrorHandler;
import errors.EvaluationError;
import identifiers.AnnotatedExpressionType;
import identifiers.ObjectType;
import identifiers.OperatorType;
import identifiers.TokenType;
import lombok.Getter;
import symbols.SymbolTable;

public final class Evaluator
{
    private final AnnotatedParseTree annotatedParseTree;
    @Getter
    private final ErrorHandler errorHandler;
    @Getter
    private final SymbolTable symbolTable;
    private Object lastValue;

    public Evaluator(TypeChecker typeChecker)
    {
        this.annotatedParseTree = typeChecker.getAnnotatedParseTree();
        this.errorHandler = typeChecker.getErrorHandler();
        this.symbolTable = typeChecker.getSymbolTable();
    }

    public Object getEvaluation()
    {
        return this.evaluate();
    }

    private Object evaluate()
    {
        try
        {
            this.evaluateStatement(this.annotatedParseTree.getStatement());
            return this.lastValue;
        } catch (Exception exception)
        {
            String message = EvaluationError.exceptionOccurred(exception);
            System.out.println(message);
            return null;
        }
    }

    private void evaluateStatement(AnnotatedStatement annotatedStatement) throws Exception
    {
        switch (annotatedStatement.getStatementType())
        {
            case ANNOTATED_BLOCK_STATEMENT:
                this.evaluateBlockStatement((AnnotatedBlockStatement) annotatedStatement);
                break;
            case ANNOTATED_EXPRESSION_STATEMENT:
                this.evaluateExpressionStatement((AnnotatedExpressionStatement) annotatedStatement);
                break;
            case ANNOTATED_CONDITIONAL_STATEMENT:
                this.evaluateConditionalStatement((AnnotatedConditionalStatement) annotatedStatement);
                break;
            case ANNOTATED_LOOP_STATEMENT:
                this.evaluateLoopStatement((AnnotatedLoopStatement) annotatedStatement);
                break;
            default:
                throw new Exception(EvaluationError.unexpectedStatement(annotatedStatement.getStatementType().toString()));
        }
    }

    private void evaluateBlockStatement(AnnotatedBlockStatement annotatedBlockStatement) throws Exception
    {
        for (AnnotatedStatement statement : annotatedBlockStatement.getStatements())
            this.evaluateStatement(statement);
    }

    private void evaluateExpressionStatement(AnnotatedExpressionStatement annotatedExpressionStatement) throws Exception
    {
        AnnotatedExpression expression = annotatedExpressionStatement.getExpression();
        this.lastValue = this.evaluateExpression(expression);
    }

    private void evaluateConditionalStatement(AnnotatedConditionalStatement annotatedStatement) throws Exception
    {
        Object condition = this.evaluateExpression(annotatedStatement.getAnnotatedCondition());

        if (condition instanceof Boolean)
            if ((boolean) condition)
                this.evaluateStatement(annotatedStatement.getAnnotatedThenStatement());
            else if (annotatedStatement.includesElseStatement())
                this.evaluateStatement(annotatedStatement.getAnnotatedElseClause());
    }

    private void evaluateLoopStatement(AnnotatedLoopStatement annotatedStatement) throws Exception
    {
        Object lowerBoundValue = this.evaluateExpression(annotatedStatement.getLowerBound());
        Object upperBoundValue = this.evaluateExpression(annotatedStatement.getUpperBound());

        if(lowerBoundValue instanceof Integer && upperBoundValue instanceof Integer)
        {
            for (int i = (int)lowerBoundValue; i <= (int)upperBoundValue; i++)
            {
                annotatedStatement.getSymbol().setValue(i);
                this.evaluateStatement(annotatedStatement.getBody());
            }
        }
    }

    private Object evaluateExpression(AnnotatedExpression annotatedExpression) throws Exception
    {
        AnnotatedExpressionType expressionType = annotatedExpression.getExpressionType();

        if (expressionType == AnnotatedExpressionType.ANNOTATED_LITERAL_EXPRESSION)
            return this.evaluateNumberExpression((AnnotatedLiteralExpression) annotatedExpression);

        if (expressionType == AnnotatedExpressionType.ANNOTATED_UNARY_EXPRESSION)
            return this.evaluateUnaryExpression((AnnotatedUnaryExpression) annotatedExpression);

        if (expressionType == AnnotatedExpressionType.ANNOTATED_BINARY_EXPRESSION)
            return this.evaluateBinaryExpression((AnnotatedBinaryExpression) annotatedExpression);

        if (expressionType == AnnotatedExpressionType.ANNOTATED_IDENTIFIER_EXPRESSION)
            return this.evaluateIdentifierExpression((AnnotatedIdentifierExpression) annotatedExpression);

        if (expressionType == AnnotatedExpressionType.ANNOTATED_ASSIGNMENT_EXPRESSION)
            return this.evaluateAssignmentExpression((AnnotatedAssignmentExpression) annotatedExpression);

        throw new Exception(EvaluationError.unexpectedExpression(annotatedExpression.getExpressionType().toString()));
    }

    private Object evaluateNumberExpression(AnnotatedLiteralExpression annotatedLiteralExpression)
    {
        return annotatedLiteralExpression.getValue();
    }

    private Object evaluateUnaryExpression(AnnotatedUnaryExpression annotatedUnaryExpression) throws Exception
    {
        Object operand = this.evaluateExpression(annotatedUnaryExpression.getOperand());
        ObjectType operandType = annotatedUnaryExpression.getObjectType();
        OperatorType operatorType = annotatedUnaryExpression.getOperator().getOperatorType();

        if (operandType == ObjectType.INTEGER_OBJECT)
            return this.evaluateUnaryIntegerExpression(operand, operatorType);

        if (operandType == ObjectType.DOUBLE_OBJECT)
            return this.evaluateUnaryDoubleExpression(operand, operatorType);

        if (operandType == ObjectType.BOOLEAN_OBJECT)
            return this.evaluateUnaryBooleanExpression(operand, operatorType);

        throw new Exception(EvaluationError.unexpectedUnaryObjectType(operandType.toString()));
    }

    private Object evaluateUnaryIntegerExpression(Object operand, OperatorType operatorType) throws Exception
    {
        int operandInteger = (int) operand;

        switch (operatorType)
        {
            case POSITIVE_OPERATOR:
                return operandInteger;
            case NEGATIVE_OPERATOR:
                return -operandInteger;
            case NEGATION_OPERATOR:
                return !(boolean) operand;
            default:
                throw new Exception(EvaluationError.unexpectedUnaryOperator(operatorType.toString()));
        }
    }

    private Object evaluateUnaryDoubleExpression(Object operand, OperatorType operatorType) throws Exception
    {
        double operandDouble = (double) operand;

        switch (operatorType)
        {
            case POSITIVE_OPERATOR:
                return operandDouble;
            case NEGATIVE_OPERATOR:
                return -operandDouble;
            case NEGATION_OPERATOR:
                return !(boolean) operand;
            default:
                throw new Exception(EvaluationError.unexpectedUnaryOperator(operatorType.toString()));
        }
    }

    private Object evaluateUnaryBooleanExpression(Object operand, OperatorType operatorType) throws Exception
    {
        boolean operandBoolean = (boolean) operand;

        switch (operatorType)
        {
            case NEGATION_OPERATOR:
                return !operandBoolean;
            default:
                throw new Exception(EvaluationError.unexpectedUnaryOperator(operatorType.toString()));
        }
    }

    private Object evaluateBinaryExpression(AnnotatedBinaryExpression annotatedBinaryExpression) throws Exception
    {
        Object leftOperand = this.evaluateExpression(annotatedBinaryExpression.getLeftOperand());
        Object rightOperand = this.evaluateExpression(annotatedBinaryExpression.getRightOperand());
        ObjectType leftOperandType = annotatedBinaryExpression.getLeftOperand().getObjectType();
        ObjectType rightOperandType = annotatedBinaryExpression.getRightOperand().getObjectType();
        OperatorType operatorType = annotatedBinaryExpression.getOperator().getOperatorType();

        if (leftOperandType == ObjectType.INTEGER_OBJECT && rightOperandType == ObjectType.INTEGER_OBJECT)
            return this.evaluateBinaryIntegerExpression(leftOperand, rightOperand, operatorType);

        if (leftOperandType == ObjectType.DOUBLE_OBJECT && rightOperandType == ObjectType.DOUBLE_OBJECT)
            return this.evaluateBinaryDoubleExpression(leftOperand, rightOperand, operatorType);

        if (leftOperandType == ObjectType.BOOLEAN_OBJECT && rightOperandType == ObjectType.BOOLEAN_OBJECT)
            return this.evaluateBinaryBooleanExpression(leftOperand, rightOperand, operatorType);

        if (leftOperandType == ObjectType.STRING_OBJECT && rightOperandType == ObjectType.STRING_OBJECT)
            return this.evaluateBinaryStringExpression(leftOperand, rightOperand, operatorType);

        throw new Exception(EvaluationError.unexpectedBinaryObjectTypes(leftOperandType.toString(),
                                                                        rightOperandType.toString()));
    }

    private Object evaluateBinaryIntegerExpression(Object leftOperand, Object rightOperand,
                                                   OperatorType operatorType) throws Exception
    {
        int leftInteger = (int) leftOperand;
        int rightInteger = (int) rightOperand;

        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return leftInteger + rightInteger;
            case SUBTRACTION_OPERATOR:
                return leftInteger - rightInteger;
            case MULTIPLICATION_OPERATOR:
                return leftInteger * rightInteger;
            case DIVISION_OPERATOR:
                if (rightInteger == 0) return 0;
                return leftInteger / rightInteger;
            case POWER_OPERATOR:
                return (int) Math.pow(leftInteger, rightInteger);
            case MODULO_OPERATOR:
                if (rightInteger == 0) return 0;
                return leftInteger % rightInteger;
            case GREATER_OPERATOR:
                return leftInteger > rightInteger;
            case LESS_OPERATOR:
                return leftInteger < rightInteger;
            case GREATER_EQUALS_OPERATOR:
                return leftInteger >= rightInteger;
            case LESS_EQUALS_OPERATOR:
                return leftInteger <= rightInteger;
            case EQUALS_EQUALS_OPERATOR:
                return leftInteger == rightInteger;
            case NOT_EQUALS_OPERATOR:
                return leftInteger != rightInteger;
            default:
                throw new Exception(EvaluationError.unexpectedBinaryOperator(operatorType.toString()));
        }
    }

    private Object evaluateBinaryDoubleExpression(Object leftOperand, Object rightOperand, OperatorType operatorType) throws Exception
    {
        double leftDouble = (double) leftOperand;
        double rightDouble = (double) rightOperand;

        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return leftDouble + rightDouble;
            case SUBTRACTION_OPERATOR:
                return leftDouble - rightDouble;
            case MULTIPLICATION_OPERATOR:
                return leftDouble * rightDouble;
            case DIVISION_OPERATOR:
                if (rightDouble == 0.0) return 0.0;
                return leftDouble / rightDouble;
            case POWER_OPERATOR:
                return Math.pow(leftDouble, rightDouble);
            case MODULO_OPERATOR:
                if (rightDouble == 0.0) return 0.0;
                return leftDouble % rightDouble;
            case GREATER_OPERATOR:
                return leftDouble > rightDouble;
            case LESS_OPERATOR:
                return leftDouble < rightDouble;
            case GREATER_EQUALS_OPERATOR:
                return leftDouble >= rightDouble;
            case LESS_EQUALS_OPERATOR:
                return leftDouble <= rightDouble;
            case EQUALS_EQUALS_OPERATOR:
                return leftDouble == rightDouble;
            case NOT_EQUALS_OPERATOR:
                return leftDouble != rightDouble;
            default:
                throw new Exception(EvaluationError.unexpectedBinaryOperator(operatorType.toString()));
        }
    }

    private Object evaluateBinaryBooleanExpression(Object leftOperand, Object rightOperand,
                                                   OperatorType operatorType) throws Exception
    {
        boolean leftBool = (boolean) leftOperand;
        boolean rightBool = (boolean) rightOperand;

        switch (operatorType)
        {
            case AND_OPERATOR:
                return leftBool && rightBool;
            case OR_OPERATOR:
                return leftBool || rightBool;
            case EQUALS_EQUALS_OPERATOR:
                return leftBool == rightBool;
            case NOT_EQUALS_OPERATOR:
                return leftBool != rightBool;
            default:
                throw new Exception(EvaluationError.unexpectedBinaryOperator(operatorType.toString()));
        }
    }

    private Object evaluateBinaryStringExpression(Object leftOperand, Object rightOperand, OperatorType operatorType) throws Exception
    {
        String leftString = leftOperand.toString();
        String rightString = rightOperand.toString();

        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return leftString + rightString;
            case EQUALS_EQUALS_OPERATOR:
                return leftString.equals(rightString);
            case NOT_EQUALS_OPERATOR:
                return !leftString.equals(rightString);
            default:
                throw new Exception(EvaluationError.unexpectedBinaryOperator(operatorType.toString()));
        }
    }

    private Object evaluateAssignmentExpression(AnnotatedAssignmentExpression annotatedAssignmentExpression) throws Exception
    {
        String name = annotatedAssignmentExpression.getName();
        Object value = this.evaluateExpression(annotatedAssignmentExpression.getExpression());
        ObjectType valueType = annotatedAssignmentExpression.getObjectType();
        TokenType tokenType = annotatedAssignmentExpression.getOperator().getTokenType();
        OperatorType operatorType = annotatedAssignmentExpression.getOperator().getOperatorType();

        if (this.symbolTable.containsSymbol(name) && tokenType != TokenType.EQUALS_TOKEN)
        {
            Object symbolValue = this.symbolTable.getSymbol(name).getValue();
            ObjectType symbolType = this.symbolTable.getSymbol(name).getType();

            if (valueType == ObjectType.INTEGER_OBJECT)
                value = evaluateAssignmentIntegerExpression(operatorType, symbolValue, value);
            else if (valueType == ObjectType.DOUBLE_OBJECT)
                value = evaluateAssignmentDoubleExpression(operatorType, symbolValue, value);
            else if (valueType == ObjectType.STRING_OBJECT)
                value = evaluateAssignmentStringExpression(operatorType, symbolValue, value);
            else
                throw new Exception(EvaluationError.unexpectedAssignmentObjectTypes(symbolType.toString(),
                                                                                    valueType.toString()));
        }
        this.symbolTable.addSymbol(name, value, valueType);
        return value;
    }

    private Object evaluateAssignmentIntegerExpression(OperatorType operatorType, Object symbolValue, Object value) throws Exception
    {
        int symbolInteger = (int) symbolValue;
        int valueInteger = (int) value;

        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return symbolInteger + valueInteger;
            case SUBTRACTION_OPERATOR:
                return symbolInteger - valueInteger;
            case MULTIPLICATION_OPERATOR:
                return symbolInteger * valueInteger;
            case DIVISION_OPERATOR:
                if (symbolInteger == 0) return 0;
                return symbolInteger / valueInteger;
            case MODULO_OPERATOR:
                if (symbolInteger == 0) return 0;
                return symbolInteger % valueInteger;
            case POWER_OPERATOR:
                return (int) Math.pow(symbolInteger, valueInteger);
            default:
                throw new Exception(EvaluationError.unexpectedAssignmentOperator(operatorType.toString()));
        }
    }

    private Object evaluateAssignmentDoubleExpression(OperatorType operatorType, Object symbolValue, Object value) throws Exception
    {
        double symbolDouble = (double) symbolValue;
        double valueDouble = (double) value;

        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return symbolDouble + valueDouble;
            case SUBTRACTION_OPERATOR:
                return symbolDouble - valueDouble;
            case MULTIPLICATION_OPERATOR:
                return symbolDouble * valueDouble;
            case DIVISION_OPERATOR:
                if (valueDouble == 0) return 0.0;
                return symbolDouble / valueDouble;
            case MODULO_OPERATOR:
                if (valueDouble == 0) return 0.0;
                return symbolDouble % valueDouble;
            case POWER_OPERATOR:
                return Math.pow(symbolDouble, valueDouble);
            default:
                throw new Exception(EvaluationError.unexpectedAssignmentOperator(operatorType.toString()));
        }
    }

    private Object evaluateAssignmentStringExpression(OperatorType operatorType, Object symbolValue, Object value) throws Exception
    {
        String symbolString = (String) symbolValue;
        String valueString = (String) value;

        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return symbolString + valueString;
            default:
                throw new Exception(EvaluationError.unexpectedAssignmentOperator(operatorType.toString()));
        }
    }

    private Object evaluateIdentifierExpression(AnnotatedIdentifierExpression annotatedIdentifierExpression) throws Exception
    {
        String name = annotatedIdentifierExpression.getName();
        if (this.symbolTable.containsSymbol(name))
            return this.symbolTable.getSymbol(name).getValue();
        throw new Exception(EvaluationError.missingSymbol(name));
    }
}
