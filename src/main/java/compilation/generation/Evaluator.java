package compilation.generation;

import compilation.analysis.semantic.*;
import exceptions.EvaluationException;
import exceptions.Exception;
import symbols.Symbol;
import symbols.SymbolTable;
import types.AnnotatedExpressionType;
import types.AnnotatedStatementType;
import types.ObjectType;
import types.OperatorType;
import types.TokenType;

import java.util.ArrayList;
import java.util.List;

import static types.AnnotatedExpressionType.ANNOTATED_BINARY_EXPRESSION;
import static types.AnnotatedExpressionType.ANNOTATED_IDENTIFIER_EXPRESSION;
import static types.ObjectType.BOOLEAN_OBJECT;
import static types.ObjectType.DOUBLE_OBJECT;
import static types.ObjectType.INTEGER_OBJECT;
import static types.ObjectType.STRING_OBJECT;

public final class Evaluator
{
    private final TypeChecker typeChecker;
    private final SymbolTable symbolTable;
    private final boolean replMode;
    private final List<Object> output;

    public Evaluator(TypeChecker typeChecker, SymbolTable symbolTable, boolean replMode)
    {
        this.typeChecker = typeChecker;
        this.symbolTable = symbolTable;
        this.replMode = replMode;
        this.output = new ArrayList<>();
    }

    public Object[] getEvaluation() throws Exception
    {
        AnnotatedParseTree parseTree = typeChecker.getAnnotatedParseTree();
        evaluateParseTree(parseTree);
        return output.toArray();
    }

    private void evaluateParseTree(AnnotatedParseTree parseTree) throws Exception
    {
        AnnotatedStatement statement = parseTree.getStatement();
        evaluateStatement(statement);
    }

    private void evaluateStatement(AnnotatedStatement statement) throws EvaluationException
    {
        AnnotatedStatementType statementType = statement.getStatementType();

        switch (statementType)
        {
            case ANNOTATED_SOURCE_STATEMENT:
                evaluateSourceStatement((AnnotatedSourceStatement) statement);
                break;
            case ANNOTATED_EXPRESSION_STATEMENT:
                evaluateExpressionStatement((AnnotatedExpressionStatement) statement);
                break;
            case ANNOTATED_BLOCK_STATEMENT:
                evaluateBlockStatement((AnnotatedBlockStatement) statement);
                break;
            case ANNOTATED_CONDITIONAL_STATEMENT:
                evaluateConditionalStatement((AnnotatedConditionalStatement) statement);
                break;
            case ANNOTATED_LOOP_STATEMENT:
                evaluateLoopStatement((AnnotatedLoopStatement) statement);
                break;
            default:
                String errorMessage = EvaluationException.unexpectedStatement(statement.getStatementType().toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private void evaluateSourceStatement(AnnotatedSourceStatement sourceStatement) throws EvaluationException
    {
        for (AnnotatedStatement statement : sourceStatement.getStatements())
            evaluateStatement(statement);
    }

    private void evaluateExpressionStatement(AnnotatedExpressionStatement expressionStatement) throws EvaluationException
    {
        AnnotatedExpression expression = expressionStatement.getExpression();
        AnnotatedExpressionType type = expression.getExpressionType();
        Object value = evaluateExpression(expression);

        if (replMode || type == ANNOTATED_IDENTIFIER_EXPRESSION || type == ANNOTATED_BINARY_EXPRESSION)
            output.add(value);
    }

    private void evaluateBlockStatement(AnnotatedBlockStatement blockStatement) throws EvaluationException
    {
        for (AnnotatedStatement statement : blockStatement.getStatements())
            evaluateStatement(statement);
    }

    private void evaluateConditionalStatement(AnnotatedConditionalStatement conditionalStatement) throws EvaluationException
    {
        AnnotatedExpression condition = conditionalStatement.getCondition();
        ObjectType conditionType = condition.getObjectType();
        Object conditionValue = evaluateExpression(conditionalStatement.getCondition());

        if (conditionType == BOOLEAN_OBJECT)
        {
            if ((boolean) conditionValue)
                evaluateStatement(conditionalStatement.getThenStatement());
            else if (conditionalStatement.includesElseStatement())
                evaluateStatement(conditionalStatement.getElseStatement());
            return;
        }

        String errorMessage = EvaluationException.invalidConditionalType(conditionType.toString());
        throw new EvaluationException(errorMessage);
    }

    private void evaluateLoopStatement(AnnotatedLoopStatement loopStatement) throws EvaluationException
    {
        AnnotatedExpression lowerBound = loopStatement.getLowerBound();
        AnnotatedExpression upperBound = loopStatement.getUpperBound();
        ObjectType lowerType = lowerBound.getObjectType();
        ObjectType upperType = upperBound.getObjectType();
        Object lowerValue = evaluateExpression(lowerBound);
        Object upperValue = evaluateExpression(upperBound);

        AnnotatedAssignmentExpression assignment = (AnnotatedAssignmentExpression) lowerBound;
        String name = assignment.getIdentifier().getName();
        Symbol symbol = symbolTable.get(name);

        if (lowerType == INTEGER_OBJECT && upperType == INTEGER_OBJECT)
        {
            for (int i = (int) lowerValue; i <= (int) upperValue; i++)
            {
                symbol.setValue(i);
                evaluateStatement(loopStatement.getBody());
            }
            return;
        }
        else if (lowerType == DOUBLE_OBJECT && upperType == DOUBLE_OBJECT)
        {
            for (double i = (double) lowerValue; i <= (double) upperValue; i++)
            {
                symbol.setValue(i);
                evaluateStatement(loopStatement.getBody());
            }
            return;
        }

        String errorMessage = EvaluationException.invalidLoopTypes(lowerType.toString(), upperType.toString());
        throw new EvaluationException(errorMessage);
    }

    private Object evaluateExpression(AnnotatedExpression expression) throws EvaluationException
    {
        AnnotatedExpressionType expressionType = expression.getExpressionType();

        switch (expressionType)
        {
            case ANNOTATED_LITERAL_EXPRESSION:
                return evaluateNumberExpression((AnnotatedLiteralExpression) expression);
            case ANNOTATED_UNARY_EXPRESSION:
                return evaluateUnaryExpression((AnnotatedUnaryExpression) expression);
            case ANNOTATED_BINARY_EXPRESSION:
                return evaluateBinaryExpression((AnnotatedBinaryExpression) expression);
            case ANNOTATED_IDENTIFIER_EXPRESSION:
                return evaluateIdentifierExpression((AnnotatedIdentifierExpression) expression);
            case ANNOTATED_ASSIGNMENT_EXPRESSION:
                return evaluateAssignmentExpression((AnnotatedAssignmentExpression) expression);
            default:
                String errorMessage = EvaluationException.unexpectedExpression(expression.getExpressionType().toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateNumberExpression(AnnotatedLiteralExpression literalExpression)
    {
        return literalExpression.getValue();
    }

    private Object evaluateUnaryExpression(AnnotatedUnaryExpression unaryExpression) throws EvaluationException
    {
        Object operand = evaluateExpression(unaryExpression.getOperand());
        ObjectType operandType = unaryExpression.getObjectType();
        OperatorType operatorType = unaryExpression.getOperator().getOperatorType();

        switch (operandType)
        {
            case INTEGER_OBJECT:
                return evaluateUnaryIntegerExpression(operand, operatorType);
            case DOUBLE_OBJECT:
                return evaluateUnaryDoubleExpression(operand, operatorType);
            case BOOLEAN_OBJECT:
                return evaluateUnaryBooleanExpression(operand, operatorType);
            default:
                String errorMessage = EvaluationException.unexpectedUnaryObjectType(operandType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateUnaryIntegerExpression(Object operand, OperatorType operatorType) throws EvaluationException
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
                String errorMessage = EvaluationException.unexpectedUnaryOperator(operatorType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateUnaryDoubleExpression(Object operand, OperatorType operatorType) throws EvaluationException
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
                String errorMessage = EvaluationException.unexpectedUnaryOperator(operatorType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateUnaryBooleanExpression(Object operand, OperatorType operatorType) throws EvaluationException
    {
        boolean operandBoolean = (boolean) operand;

        switch (operatorType)
        {
            case NEGATION_OPERATOR:
                return !operandBoolean;
            default:
                String errorMessage = EvaluationException.unexpectedUnaryOperator(operatorType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateBinaryExpression(AnnotatedBinaryExpression binaryExpression) throws EvaluationException
    {
        Object leftOperand = evaluateExpression(binaryExpression.getLeftOperand());
        Object rightOperand = evaluateExpression(binaryExpression.getRightOperand());
        ObjectType leftOperandType = binaryExpression.getLeftOperand().getObjectType();
        ObjectType rightOperandType = binaryExpression.getRightOperand().getObjectType();
        OperatorType operatorType = binaryExpression.getOperator().getOperatorType();

        if (leftOperandType == INTEGER_OBJECT && rightOperandType == INTEGER_OBJECT)
            return evaluateBinaryIntegerExpression(leftOperand, rightOperand, operatorType);

        if (leftOperandType == DOUBLE_OBJECT && rightOperandType == DOUBLE_OBJECT)
            return evaluateBinaryDoubleExpression(leftOperand, rightOperand, operatorType);

        if (leftOperandType == BOOLEAN_OBJECT && rightOperandType == BOOLEAN_OBJECT)
            return evaluateBinaryBooleanExpression(leftOperand, rightOperand, operatorType);

        if (leftOperandType == STRING_OBJECT && rightOperandType == STRING_OBJECT)
            return evaluateBinaryStringExpression(leftOperand, rightOperand, operatorType);

        String errorMessage = EvaluationException.unexpectedBinaryObjectTypes(leftOperandType.toString(), rightOperandType.toString());
        throw new EvaluationException(errorMessage);
    }

    private Object evaluateBinaryIntegerExpression(Object leftOperand, Object rightOperand, OperatorType operatorType) throws EvaluationException
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
                String errorMessage = EvaluationException.unexpectedBinaryOperator(operatorType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateBinaryDoubleExpression(Object leftOperand, Object rightOperand, OperatorType operatorType) throws EvaluationException
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
                String errorMessage = EvaluationException.unexpectedBinaryOperator(operatorType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateBinaryBooleanExpression(Object leftOperand, Object rightOperand, OperatorType operatorType) throws EvaluationException
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
                String errorMessage = EvaluationException.unexpectedBinaryOperator(operatorType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateBinaryStringExpression(Object leftOperand, Object rightOperand, OperatorType operatorType) throws EvaluationException
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
                String errorMessage = EvaluationException.unexpectedBinaryOperator(operatorType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateAssignmentExpression(AnnotatedAssignmentExpression annotatedAssignmentExpression) throws EvaluationException
    {
        String name = annotatedAssignmentExpression.getIdentifier().getName();
        Object value = evaluateExpression(annotatedAssignmentExpression.getExpression());
        ObjectType valueType = annotatedAssignmentExpression.getObjectType();
        TokenType tokenType = annotatedAssignmentExpression.getOperator().getTokenType();
        OperatorType operatorType = annotatedAssignmentExpression.getOperator().getOperatorType();

        if (symbolTable.contains(name) && tokenType != TokenType.EQUALS_TOKEN)
        {
            Object symbolValue = symbolTable.get(name).getValue();
            ObjectType symbolType = symbolTable.get(name).getType();

            switch (valueType)
            {
                case INTEGER_OBJECT:
                    value = evaluateIntegerAssignmentExpression(operatorType, symbolValue, value);
                    break;
                case DOUBLE_OBJECT:
                    value = evaluateDoubleAssignmentExpression(operatorType, symbolValue, value);
                    break;
                case STRING_OBJECT:
                    value = evaluateStringAssignmentExpression(operatorType, symbolValue, value);
                    break;
                default:
                    String errorMessage = EvaluationException.unexpectedAssignmentObjectTypes(symbolType.toString(), valueType.toString());
                    throw new EvaluationException(errorMessage);
            }
        }
        symbolTable.add(name, value, valueType);

        return value;
    }

    private Object evaluateIntegerAssignmentExpression(OperatorType operatorType, Object symbolValue, Object value) throws EvaluationException
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
                String errorMessage = EvaluationException.unexpectedAssignmentOperator(operatorType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateDoubleAssignmentExpression(OperatorType operatorType, Object symbolValue, Object value) throws EvaluationException
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
                String errorMessage = EvaluationException.unexpectedAssignmentOperator(operatorType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateStringAssignmentExpression(OperatorType operatorType, Object symbolValue, Object value) throws EvaluationException
    {
        String symbolString = (String) symbolValue;
        String valueString = (String) value;

        switch (operatorType)
        {
            case ADDITION_OPERATOR:
                return symbolString + valueString;
            default:
                String errorMessage = EvaluationException.unexpectedAssignmentOperator(operatorType.toString());
                throw new EvaluationException(errorMessage);
        }
    }

    private Object evaluateIdentifierExpression(AnnotatedIdentifierExpression annotatedIdentifierExpression) throws EvaluationException
    {
        String name = annotatedIdentifierExpression.getName();
        if (symbolTable.contains(name))
            return symbolTable.get(name).getValue();

        String errorMessage = EvaluationException.missingSymbol(name);
        throw new EvaluationException(errorMessage);
    }
}
