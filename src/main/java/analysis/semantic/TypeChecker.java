package analysis.semantic;

import analysis.syntax.*;
import errors.ErrorHandler;
import errors.SemanticError;
import identifiers.ObjectType;
import symbols.SymbolTable;
import util.Utilities;

import java.util.HashMap;

public final class TypeChecker
{
    private final ParseTree parseTree;
    private final ErrorHandler errorHandler;
    private final HashMap<String, Object> variables;

    public TypeChecker(Parser parser, ErrorHandler errorHandler, SymbolTable symbolTable)
    {
        this.parseTree = parser.getParseTree();
        this.errorHandler = errorHandler;
        this.variables = symbolTable.getVariables();
    }

    public AnnotatedParseTree getAnnotatedParseTree()
    {
        return new AnnotatedParseTree(this.getAnnotatedExpression());
    }

    public HashMap<String, Object> getVariables()
    {
        return variables;
    }

    private AnnotatedExpression getAnnotatedExpression()
    {
        return this.annotate(this.parseTree.getExpression());
    }

    private AnnotatedExpression annotate(Expression syntax)
    {
        try
        {
            return this.annotateExpression(syntax);
        }
        catch(Exception error)
        {
            System.out.println(error.getMessage());
            return null;
        }
    }

    private AnnotatedExpression annotateExpression(Expression syntax) throws Exception
    {
        switch(syntax.getTokenType())
        {
            case PARENTHESIZED_EXPRESSION_TOKEN:
                return this.annotateParenthesizedExpression((ParenthesizedExpression)syntax);
            case LITERAL_EXPRESSION_TOKEN:
                return this.annotateLiteralExpression((LiteralExpression)syntax);
            case UNARY_EXPRESSION_TOKEN:
                return this.annotateUnaryExpression((UnaryExpression)syntax);
            case BINARY_EXPRESSION_TOKEN:
                return this.annotateBinaryExpression((BinaryExpression)syntax);
            case NAME_EXPRESSION_TOKEN:
                return this.annotateNameExpression((NameExpression)syntax);
            case ASSIGNMENT_EXPRESSION_TOKEN:
                return this.annotateAssignmentExpression((AssignmentExpression)syntax);
            default:
                throw new Exception(String.format("Unexpected syntax '%s'", syntax.getTokenType()));
        }
    }

    private AnnotatedExpression annotateParenthesizedExpression(ParenthesizedExpression syntax) throws Exception
    {
        return this.annotateExpression(syntax.getExpression());
    }

    private AnnotatedExpression annotateLiteralExpression(LiteralExpression syntax)
    {
        Object value = syntax.getValue();
        if(value == null)
            value = 0;

        return new AnnotatedLiteralExpression(value);
    }

    private AnnotatedExpression annotateUnaryExpression(UnaryExpression syntax) throws Exception
    {
        AnnotatedExpression annotatedOperand = this.annotateExpression(syntax.getOperand());
        AnnotatedUnaryOperator annotatedOperator = TypeBinder.bindUnaryOperators(syntax.getOperatorToken().getTokenType(), annotatedOperand.getObjectType());

        if(annotatedOperator == null)
        {
            this.errorHandler.addError(SemanticError.undefinedUnaryOperator(syntax.getOperatorToken().getSpan(), syntax.getOperatorToken().getSyntax(), annotatedOperand.getObjectType()));
            return annotatedOperand;
        }

        return new AnnotatedUnaryExpression(annotatedOperator, annotatedOperand);
    }

    private AnnotatedExpression annotateBinaryExpression(BinaryExpression syntax) throws Exception
    {
        AnnotatedExpression annotatedLeft = this.annotateExpression(syntax.getLeftTerm());
        AnnotatedExpression annotatedRight = this.annotateExpression(syntax.getRightTerm());
        AnnotatedBinaryOperator annotatedOperator = TypeBinder.bindBinaryOperators(syntax.getOperatorToken().getTokenType(), annotatedLeft.getObjectType(), annotatedRight.getObjectType());

        if(annotatedOperator == null)
        {
            this.errorHandler.addError(SemanticError.undefinedBinaryOperator(syntax.getOperatorToken().getSpan(), syntax.getOperatorToken().getSyntax(), annotatedLeft.getObjectType(), annotatedRight.getObjectType()));
            return annotatedLeft;
        }

        return new AnnotatedBinaryExpression(annotatedLeft, annotatedOperator, annotatedRight);
    }

    private AnnotatedExpression annotateNameExpression(NameExpression syntax)
    {
        String name = syntax.getIdentifierToken().getSyntax();

        if(!this.variables.containsKey(name))
        {
            this.errorHandler.addError(SemanticError.undefinedName(syntax.getIdentifierToken().getSpan(), name));
            return new AnnotatedLiteralExpression(null);
        }
        ObjectType type = Utilities.typeOf(this.variables.get(name));

        return new AnnotatedVariableExpression(name, type);
    }

    private AnnotatedExpression annotateAssignmentExpression(AssignmentExpression syntax) throws Exception
    {
        String name = syntax.getIdentifierToken().getSyntax();
        AnnotatedExpression expression = this.annotateExpression(syntax.getExpression());
        return new AnnotatedAssignmentExpression(name, expression);
    }
}
